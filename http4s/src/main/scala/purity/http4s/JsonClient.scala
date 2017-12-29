package purity.http4s

import cats.implicits._
import cats.data.EitherT
import cats.effect.{Effect, Sync}
import io.circe.{Decoder, Encoder, Json, Printer}
import org.http4s.{EntityDecoder, InvalidMessageBodyFailure, MessageFailure, Request, Response, Status, Uri}
import org.http4s.client.Client
import org.http4s.circe.CirceInstances
import org.http4s.dsl.Http4sDsl
import org.http4s.headers.{Accept, MediaRangeAndQValue}
import purity.config.ConfigContainer
import purity.http4s.JsonClient.ServiceError
import purity.logging.{LogLine, LoggerContainer}
import purity.script.ScriptDsl

object JsonClient {

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
case class JsonClient[F[+_]](
    client: F[Client[F]],
    jsonPrinter: Printer = JsonClient.DefaultJsonPrinter)
    (implicit effect: Effect[F])
  extends ScriptDsl[F] with Http4sDsl[F] {

  private val circe: CirceInstances = new CirceInstances {
    override protected def defaultPrinter: Printer = jsonPrinter
    override implicit def jsonDecoder[F[_]](implicit ev: Sync[F]): EntityDecoder[F, Json] = CirceInstances.defaultJsonDecoder
  }

  def shutdown: F[Unit] = client.flatMap(_.shutdown)

  def on(uri: Uri): OnUri = OnUri(uri)

  def on(uri: CantFail[ConfigContainer, Uri]): OnUriWithConfig = OnUriWithConfig(uri)

  def fetchAs[A](request: F[Request[F]])(implicit decoder: Decoder[A], fl: sourcecode.File, cl: sourcecode.Line): Script[LoggerContainer[F], ServiceError[F], A] = {
    for {
      r <- liftF(request)
      from = s"${r.method.name}: ${r.uri.renderString}"
      _ <- log.debug(s"Requesting $from")(effect, fl, cl)
      a <- liftFE(fire(r))
        .logFailure { e =>
          LogLine.error(s"($from) -> (response status code: ${e.response.status.code}): ${e.failure.message}", e.failure)(fl, cl)
        }
      _ <- log.debug(s"Response ${a._3.code} from $from")(effect, fl, cl)
      _ <- log.trace(s"POST json response: ${a._2}")(effect, fl, cl)
    } yield a._1
  }

  def fetchAs[A](request: Request[F])(implicit decoder: Decoder[A], fl: sourcecode.File, cl: sourcecode.Line): Script[LoggerContainer[F], ServiceError[F], A] =
    fetchAs(effect.pure(request))(decoder, fl, cl)

  /** Same as client.fetchAs from http4s, but it lifts the Message error into an F[Either[ServiceError[F], A]]. */
  private def fire[A](request: Request[F])(implicit decoder: Decoder[A]): F[Either[ServiceError[F], (A, Json, Status)]] = {
    val m = circe.jsonDecoder.consumes.toList
    val r = request.putHeaders(Accept(MediaRangeAndQValue(m.head), m.tail.map(MediaRangeAndQValue(_)): _*))
    client.flatMap(_.fetch(r) { response =>
      val jsonAndA =
        for {
          json <- circe.jsonDecoder.decode(response, strict = false)
          decodeLift = EitherT[F, io.circe.DecodingFailure, A](effect.pure(decoder(json.hcursor)))
          failureMapped: EitherT[F, org.http4s.DecodeFailure, A] = decodeLift.leftMap { failure: io.circe.DecodingFailure =>
            InvalidMessageBodyFailure(s"Could not decode JSON: $json", Some(failure))
          }
          a <- failureMapped
        } yield (a, json, response.status)
      jsonAndA.leftMap(e => ServiceError(request, response, e)).value
    })
  }

  case class OnUri(uri: Uri) {

    def get[A](implicit decoder: Decoder[A], fl: sourcecode.File, cl: sourcecode.Line): Script[LoggerContainer[F], ServiceError[F], A] =
      fetchAs[A](Request[F](method = GET, uri = uri))(decoder, fl, cl)

    def getWithDecoder[A](decoder: Decoder[A])(implicit fl: sourcecode.File, cl: sourcecode.Line): Script[LoggerContainer[F], ServiceError[F], A] =
      get(decoder, fl, cl)

    def post[A](body: A): PartiallyAppliedRequest[A] =
      PartiallyAppliedRequest[A](uri, body)
  }

  case class PartiallyAppliedRequest[A](uri: Uri, body: A) {

    def andExpect[B](implicit encoder: Encoder[A], decoder: Decoder[B], fl: sourcecode.File, cl: sourcecode.Line): Script[LoggerContainer[F], ServiceError[F], B] =
      for {
        _ <- log.trace(s"POST json request: ${encoder(body).pretty(Printer.spaces2)}")(effect, fl, cl)
        response <- fetchAs[B](Request.apply[F](method = POST, uri = uri).withBody(body)(effect, circe.jsonEncoderOf))(decoder, fl, cl)
      } yield response

    def andExpectWithDecoder[B](decoder: Decoder[B])(implicit encoder: Encoder[A], fl: sourcecode.File, cl: sourcecode.Line): Script[LoggerContainer[F], ServiceError[F], B] =
      andExpect(encoder, decoder, fl, cl)
  }

  case class OnUriWithConfig(uri: CantFail[ConfigContainer, Uri]) {

    def get[A](implicit decoder: Decoder[A], fl: sourcecode.File, cl: sourcecode.Line): Script[LoggerContainer[F] with ConfigContainer, ServiceError[F], A] =
      for {
        endpoint <- uri
        response ← on(endpoint).get[A](decoder, fl, cl)
      } yield response

    def getWithDecoder[A](decoder: Decoder[A])(implicit fl: sourcecode.File, cl: sourcecode.Line): Script[LoggerContainer[F] with ConfigContainer, ServiceError[F], A] =
      get(decoder, fl, cl)

    def post[A](body: A): PartiallyAppliedRequestWithConfig[A] =
      new PartiallyAppliedRequestWithConfig[A](uri, body)
  }

  class PartiallyAppliedRequestWithConfig[A](uri: CantFail[ConfigContainer, Uri], body: A) {

    def andExpect[B](implicit encoder: Encoder[A], decoder: Decoder[B], fl: sourcecode.File, cl: sourcecode.Line): Script[LoggerContainer[F] with ConfigContainer, ServiceError[F], B] =
      for {
        endpoint <- uri
        response <- on(endpoint).post(body).andExpect[B](encoder, decoder, fl, cl)
      } yield response

    def andExpectWithDecoder[B](decoder: Decoder[B])(implicit encoder: Encoder[A], fl: sourcecode.File, cl: sourcecode.Line): Script[LoggerContainer[F] with ConfigContainer, ServiceError[F], B] =
      andExpect(encoder, decoder, fl, cl)
  }
}

