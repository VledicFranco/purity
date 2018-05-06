package purity.script

import cats.effect.IO
import org.scalatest.BeforeAndAfterEach
import purity.ScriptSuite
import purity.logging._

class ScriptTSuite extends ScriptSuite[IO] with BeforeAndAfterEach {

  /*
  val console: MutableConsole = MutableConsole(LogLevel.DebugLevel)

  implicit val logger: LoggerContainer[IO] = console.loggerContainer

  override def beforeEach(): Unit = console.buffer.clear()

  describe("ScriptT") {

    it("logs") {
      proveThatAfter(log.info("hola")).itHoldsThat(console)(MutableConsole.hasAmountOfLines(1))
    }

    it("logs failed scripts") {
      val script = raiseFailure("error").logFailure(LogLine.info).recoverFailure(x => pure(x))
      proveThatAfter(script).itHoldsThat(console)(MutableConsole.hasAmountOfLines(1))
    }

    it("logs error scripts") {
      val script = raiseError(new RuntimeException("hola")).logError(LogLine.info).recoverError(e => pure(e))
      proveThatAfter(script).itHoldsThat(console)(MutableConsole.hasAmountOfLines(1))
    }
  }
  */
}
