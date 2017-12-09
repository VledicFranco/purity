package purity.http4s

import cats.effect.IO
import org.http4s.client.Client
import org.http4s.dsl.io.uri
import purity.http4s.JsonEchoService.{Ping, Pong}
import purity.http4s.JsonClient.ServiceError
import purity.ScriptSuite
import purity.logging.{LogLevel, LoggerFunction, MutableConsole}

class JsonClientSpec extends ScriptSuite[IO] {

  val jsonClient: JsonClient[IO] = JsonClient(Client.fromHttpService(JsonEchoService.service))

  val console: MutableConsole = MutableConsole(LogLevel.DebugLevel)

  implicit val logger: LoggerFunction = console.logger

  val response: Independent[ServiceError[IO], Pong] = jsonClient.on(uri("/ping")).post(Ping("pang")).andExpect[Pong]

  describe("JsonClient") {

    it("logs the request information") {
      proveThatAfter(response).itHoldsThat(console)(MutableConsole.hasAmountOfLines(3))
    }
  }
}

