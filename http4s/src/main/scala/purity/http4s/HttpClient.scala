package purity.http4s

import cats.Monad
import cats.effect.IO
import io.circe.{Decoder, Encoder, Json, Printer}
import org.http4s.{EntityEncoder, Message, Request, Response, Uri}
import org.http4s.client.Client
import org.http4s.dsl.io._
import org.http4s.circe.{jsonDecoder, jsonEncoderWithPrinter}
import purity.logging.LogLine
import purity.script.io.{ Script, script, log }

/**
  * Intended to be used inside services which are dependencies.
  * - Lifts http4s client operations into the Script type.
  * - Adds logging between requests and responses.
  * - Adds possible IO failures: ServiceUnexpectedStatus, ServiceUnexpectedEntity and ServiceUnexpectedJson
  */
case class HttpClient(client: Client[IO]) {

  def getAndExpect[A](uri: Uri)(implicit decoder: Decoder[A]): Script[Any, Nothing, A] =
    fetchWithLogging[A](Request[IO](method = GET, uri = uri))

  def postAndExpect[A]: PartiallyAppliedPost[A] = new PartiallyAppliedPost[A]

  protected val clientPrinter: Printer =
    Printer.noSpaces.copy(dropNullValues = true)

  private val prettyEncoder: EntityEncoder[IO, Json] =
    jsonEncoderWithPrinter(clientPrinter)

  private def prettyBody[A](message: Message[IO], body: A)(implicit circeEncoder: Encoder[A]): IO[message.Self] =
    message.withBody(body)(Monad[IO], prettyEncoder.contramap(circeEncoder.apply))

  private def fetchWithLogging[A](request: Request[IO])(implicit decoder: Decoder[A]): Script[Any, Nothing, A] =
    fetchWithLogging[A](IO.pure(request))

  private def fetchWithLogging[A](requestIO: IO[Request[IO]])(implicit decoder: Decoder[A]): Script[Any, Nothing, A] = {
    (for {
      request ← script(requestIO)
      _ ← log.debug(s"${request.method.name}: ${request.uri.renderString}")
      result ← script(client.fetch(request)(decodeResponse[A](request.uri, _)))
    } yield result)
      .logError {
        case e @ ServiceUnexpectedEntity(uri, message) ⇒
          LogLine.error(s"Unexpected body entity on ${uri.renderString} $message", e)
        case e @ ServiceUnexpectedStatus(uri, json, status) ⇒
          LogLine.error(s"Unexpected status ${status.code} on ${uri.renderString} with json $json", e)
        case e @ ServiceUnexpectedModel(uri, json, _) ⇒
          LogLine.error(s"Unexpected json response on ${uri.renderString}\n$json", e)
        case other ⇒
          LogLine.error(other)
      }
  }

  private def decodeResponse[A](uri: Uri, response: Response[IO])(implicit decoder: Decoder[A]): IO[A] =
    for {
      failureOrJson ← jsonDecoder[IO].decode(response, strict = false).value
      json ← failureOrJson match {
        case Left(e) ⇒
          IO.raiseError(ServiceUnexpectedEntity(uri, e.message))
        case Right(json) ⇒
          IO.pure(json)
      }
      _ <-
        if (response.status.isSuccess) IO.pure(())
        else IO.raiseError(ServiceUnexpectedStatus(uri, json, response.status))
      result ← decoder(json.hcursor) match {
        case Left(e) ⇒
          IO.raiseError(ServiceUnexpectedModel(uri, json, e))
        case Right(a1) ⇒
          IO.pure(a1)
      }
    } yield result

  class PartiallyAppliedPost[A] {

    def apply[B](uri: Uri, body: B)(implicit decoder: Decoder[A], encoder: Encoder[B]): Script[Any, Nothing, A] =
      for {
        _ ← log.trace("Posting: " + encoder(body).pretty(clientPrinter))
        result ← fetchWithLogging[A](prettyBody(Request[IO](method = POST, uri = uri), body))
      } yield result
  }
}


