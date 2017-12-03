package purity.http4s

import cats.effect.Effect
import io.circe.{Decoder, Encoder, Json, Printer}
import org.http4s.{EntityDecoder, MessageFailure, Request, Response, Uri}
import org.http4s.client.{Client, RequestKey}
import org.http4s.circe.CirceInstances
import org.http4s.client.blaze.{BlazeClientConfig, PooledHttp1Client}
import org.http4s.dsl.Http4sDsl
import org.http4s.headers.{Accept, MediaRangeAndQValue}
import purity.http4s.JsonPooledClient.ServiceError
import purity.logging.LogLine
import purity.script.ScriptDsl

object JsonPooledClient {

  val DefaultMaxTotalConnections = 10

  val DefaultMaxWaitQueueLimit = 256

  val DefaultJsonPrinter: Printer =
    Printer.noSpaces.copy(dropNullValues = true)

  case class ServiceError[F[_]](request: Request[F], response: Response[F], failure: MessageFailure) extends RuntimeException
}

/**
  * Extra layer on top of a Blaze pooled http client.
  * - Lifts http4s client operations to the Script type.
  * - Adds logging between requests and responses.
  * - Lifts possible ServiceFailures to the Script type.
  */
case class JsonPooledClient[F[+_]](
                                    maxTotalConnections: Int = JsonPooledClient.DefaultMaxTotalConnections,
                                    maxWaitQueueLimit: Int = JsonPooledClient.DefaultMaxWaitQueueLimit,
                                    maxConnectionsPerRequestKey: RequestKey => Int = _ => JsonPooledClient.DefaultMaxTotalConnections,
                                    config: BlazeClientConfig = BlazeClientConfig.defaultConfig,
                                    jsonPrinter: Printer = JsonPooledClient.DefaultJsonPrinter)
                                  (implicit effect: Effect[F])
  extends ScriptDsl[F] with Http4sDsl[F] {

  private val circe: CirceInstances = new CirceInstances {
    override protected def defaultPrinter: Printer = jsonPrinter
    override implicit def jsonDecoder[G[_]: Effect]: EntityDecoder[G, Json] = CirceInstances.defaultJsonDecoder
  }

  private val clientPool: Client[F] = PooledHttp1Client[F](
    maxTotalConnections = maxTotalConnections,
    maxConnectionsPerRequestKey = maxConnectionsPerRequestKey,
    maxWaitQueueLimit = maxWaitQueueLimit,
    config = config)

  def shutDownClientPool: F[Unit] = clientPool.shutdown

  def on(uri: Uri): OnUri = OnUri(uri)

  def fetchAs[A](request: F[Request[F]])(implicit decoder: Decoder[A]): Script[Any, ServiceError[F], A] = {
    for {
      r <- liftF(request)
      from = s"${r.method.name}: ${r.uri.renderString}"
      _ <- log.debug(from)
      a <- liftFE(fire(r)(circe.jsonOf[F, A]))
        .logFailure { e =>
          LogLine.error(s"($from) -> (response status code: ${e.response.status.code}): ${e.failure.message}", e.failure)
        }
    } yield a
  }

  def fetchAs[A](request: Request[F])(implicit decoder: Decoder[A]): Script[Any, ServiceError[F], A] =
    fetchAs(effect.pure(request))

  /** Same as client.fetchAs from http4s, but it lifts the Message error into an F[Either[ServiceError[F], A]]. */
  private def fire[A](request: Request[F])(implicit d: EntityDecoder[F, A]): F[Either[ServiceError[F], A]] = {
    val r = if (d.consumes.nonEmpty) {
      val m = d.consumes.toList
      request.putHeaders(Accept(MediaRangeAndQValue(m.head), m.tail.map(MediaRangeAndQValue(_)): _*))
    } else request
    clientPool.fetch(r) { response =>
      d.decode(response, strict = false).leftMap(e => ServiceError(request, response, e)).value
    }
  }

  case class OnUri(uri: Uri) {

    def get[A](implicit decoder: Decoder[A]): Script[Any, ServiceError[F], A] =
      fetchAs[A](Request[F](method = GET, uri = uri))

    def post[A](body: A): PartiallyAppliedRequest[A] =
      PartiallyAppliedRequest[A](uri, body)
  }

  case class PartiallyAppliedRequest[A](uri: Uri, body: A) {

    def andExpect[B](implicit encoder: Encoder[A], decoder: Decoder[B]): Script[Any, ServiceError[F], B] =
      fetchAs[B](Request.apply[F](method = POST, uri = uri).withBody(body)(effect, circe.jsonEncoderOf))
  }
}

