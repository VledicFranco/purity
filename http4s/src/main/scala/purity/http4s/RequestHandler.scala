package purity.http4s

import cats.FlatMap
import cats.effect.IO
import purity.logging.LoggerFunction
import purity.script.io._
import io.circe.syntax._
import io.circe._
import org.http4s._
import org.http4s.circe._
import org.http4s.dsl.io._

case class RequestHandler[D](errorHandler: Throwable => IO[Response[IO]])(implicit config: D, logger: LoggerFunction) {

  /**
    * Runs a Script function (A => Script[D, E, B]) to handle an http request and produce a response for http4s.
    * This function also handle format of responses and handles lower level errors (failed IO monads).
    * The configuration and logger required by the Script should be implicitly provided, as well as a function that should
    * map the Script failures to valid http4s responses.
    */
  def apply[Req: Decoder, Res: Encoder, E](request: Request[IO], f: Req ⇒ Script[D, E, Res])(failureHandler: E ⇒ IO[Response[IO]]): IO[Response[IO]] = {
    val script: IO[Script[D, E, Res]] = request.as[Req](FlatMap[IO], jsonOf[IO, Req]).map(f)
    val response = script.flatMap(_.foldF(
      config, logger, failureHandler, res ⇒ Ok(res.asJson)))
    response.attempt.flatMap {
      case Left(e) => errorHandler(e)
      case Right(response1) => IO.pure(response1)
    }
  }

  def noFailure[Req: Decoder, Res: Encoder](request: Request[IO], f: Req ⇒ Script[D, Nothing, Res]): IO[Response[IO]] =
    this.apply(request, f)(_ => Ok("Unreachable"))
}
