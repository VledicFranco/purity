package purity.http4s

import cats.Monad
import cats.effect.IO
import io.circe.{Decoder, Encoder}
import io.circe.syntax._
import org.http4s._
import org.http4s.circe._
import org.http4s.dsl.io._

object JsonEchoService {

  case class Ping(message: String)

  object Ping {
    implicit val jsonDecoder: Decoder[Ping] =
      Decoder.forProduct1("message")(Ping.apply)
    implicit val jsonEncoder: Encoder[Ping] =
      Encoder.forProduct1("message")(_.message)
  }

  case class Pong(message: String)

  object Pong {
    implicit val jsonDecoder: Decoder[Pong] =
      Decoder.forProduct1("message")(Pong.apply)
    implicit val jsonEncoder: Encoder[Pong] =
      Encoder.forProduct1("message")(_.message)
  }

  val service: HttpService[IO] =
    HttpService {
      case request@POST -> Root / "ping" =>
        request.as(Monad[IO], jsonOf[IO, Ping]).flatMap(p => Ok(Pong(s"${p.message}!").asJson))
    }
}
