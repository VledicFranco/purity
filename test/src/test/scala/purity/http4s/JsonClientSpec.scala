package purity.http4s

import cats.effect.IO
import com.typesafe.config.{Config, ConfigFactory}
import org.http4s.Uri
import org.http4s.client.Client
import org.http4s.dsl.io.uri
import purity.http4s.JsonEchoService.{Ping, Pong}
import purity.http4s.JsonClient.ServiceError
import purity.ScriptSuite
import purity.config.ConfigContainer
import purity.logging.{LogLevel, Logger, LoggerContainer, MutableConsole}

class JsonClientSpec extends ScriptSuite[IO] {

  val client: JsonClient[IO] = JsonClient(IO(Client.fromHttpService(JsonEchoService.service)))

  val console0: MutableConsole = MutableConsole(LogLevel.AllLevel)

  val console1: MutableConsole = MutableConsole(LogLevel.AllLevel)

  val uriScript: CantFail[ConfigContainer, Uri] =
    dependencies[ConfigContainer].map(c => Uri.fromString(c.config.getString("service.uri")).right.get)

  val response0: Script[LoggerContainer[IO], ServiceError[IO], Pong] =
    client.on(uri("/ping")).post(Ping("pang")).andExpect[Pong]

  val response1: Script[ServiceTools[IO], ServiceError[IO], Pong] =
    for {
      tools <- dependencies[JsonClientContainer[IO]]
      client <- tools.jsonClient
      pong <- client.on(uriScript).post(Ping("pang")).andExpect[Pong]
    } yield pong

  describe("JsonClient") {

    it("logs the request information") {
      implicit val logger: LoggerContainer[IO] = console0.loggerContainer
      proveThatAfter(response0).itHoldsThat(console0)(MutableConsole.hasAmountOfLines(4))
    }

    it("logs the request information with config uris") {
      implicit val loggerWithConfig: ServiceTools[IO] =
        new ServiceTools[IO] {
          override def logger: Logger[IO] = console1.logger
          override def config: Config = ConfigFactory.parseString(""" service.uri = "/ping" """)
          override def jsonClient: CantFail[ConfigContainer, JsonClient[IO]] = pure(client)
        }
      proveThatAfter(response1).itHoldsThat(console1)(MutableConsole.hasAmountOfLines(4))
    }
  }
}

