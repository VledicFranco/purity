package purity.http4s

import cats.implicits._
import cats.effect.Effect
import purity.logging.LoggerFunction
import io.circe._
import io.circe.syntax._
import org.http4s._
import org.http4s.circe.CirceInstances
import org.http4s.dsl.Http4sDsl
import purity.http4s.JsonRequestHandler.ErrorHandler
import purity.script.ScriptDsl

object JsonRequestHandler {

  def DefaultErrorHandler[F[_]: Effect]: ErrorHandler[F] = {
    val dsl = new Http4sDsl[F] {}
    import dsl._
    ErrorHandler {
      case (request, e, logger) =>
        logger.fatal(s"Uncaught exception when handling request: ${request.method.name} ${request.uri.renderString}", e)
        dsl.InternalServerError()
    }
  }

  val DefaultJsonPrinter: Printer =
    Printer.noSpaces.copy(dropNullValues = true)

  case class ErrorHandler[F[_]](run: PartialFunction[(Request[F], Throwable, LoggerFunction), F[Response[F]]]) {

    def check(request: Request[F], throwable: Throwable, loggerFunction: LoggerFunction): F[Response[F]] =
      run((request, throwable, loggerFunction))

    def orElse(that: ErrorHandler[F]): ErrorHandler[F] =
      ErrorHandler(run.orElse(that.run))
  }
}

case class JsonRequestHandler[F[+_]](
                                      errorHandler: ErrorHandler[F],
                                      jsonPrinter: Printer = JsonRequestHandler.DefaultJsonPrinter)
                                    (implicit effect: Effect[F])
  extends ScriptDsl[F] with Http4sDsl[F] {

  private val circeInstances: CirceInstances = new CirceInstances {
    override protected def defaultPrinter: Printer = jsonPrinter
    override implicit def jsonDecoder[G[_]: Effect]: EntityDecoder[G, Json] = CirceInstances.defaultJsonDecoder
  }

  import circeInstances._

  /**
    * Runs a Script function (A => Script[D, E, B]) to handle an http request and produce a response for http4s.
    * This function also handle format of responses and handles lower level errors (failed F monads).
    * The configuration and logger required by the Script should be implicitly provided, as well as a function that should
    * map the Script failures to valid http4s responses.
    */
  def apply[Req: Decoder, Res: Encoder, D, E](request: Request[F], f: Req ⇒ Script[D, E, Res])(failureHandler: E ⇒ F[Response[F]])(implicit config: D, logger: LoggerFunction): F[Response[F]] = {
    val script: F[Script[D, E, Res]] = request.as[Req](Effect[F], jsonOf[F, Req]).map(f)
    val response = script.flatMap(_.foldF(
      config, logger, failureHandler, res ⇒ Ok(res.asJson)))
    response.attempt.flatMap {
      case Left(e) => errorHandler.check(request, e, logger)
      case Right(response1) => effect.pure(response1)
    }
  }

  def noFailure[Req: Decoder, Res: Encoder, D](request: Request[F], f: Req ⇒ Script[D, Nothing, Res])(implicit config: D, logger: LoggerFunction): F[Response[F]] =
    this.apply(request, f)(_ => InternalServerError("This should be unreachable"))
}

