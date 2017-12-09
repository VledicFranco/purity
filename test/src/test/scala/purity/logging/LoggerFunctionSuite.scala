package purity.logging

import purity.PuritySuite
import Generators._

class LoggerFunctionSuite extends PuritySuite {

  test("LoggerFunction.log with level All") {
    forAll { lines: List[LogLine] ⇒
      val console = MutableConsole(LogLevel.AllLevel)
      lines.foreach(console.logger.log)
      console.lines.length shouldEqual lines.length
    }
  }

  test("LoggerFunction.log with level Fatal") {
    forAll { lines: List[LogLine] ⇒
      val console = MutableConsole(LogLevel.FatalLevel)
      lines.foreach(console.logger.log)
      console.lines.length shouldEqual lines.count {
        case _: LogLine.Fatal => true
        case _ => false
      }
    }
  }

  test("LoggerFunction.log with level Error") {
    forAll { lines: List[LogLine] ⇒
      val console = MutableConsole(LogLevel.ErrorLevel)
      lines.foreach(console.logger.log)
      console.lines.length shouldEqual lines.count {
        case _: LogLine.Fatal => true
        case _: LogLine.Error => true
        case _ => false
      }
    }
  }

  test("LoggerFunction.log with level Warn") {
    forAll { lines: List[LogLine] ⇒
      val console = MutableConsole(LogLevel.WarnLevel)
      lines.foreach(console.logger.log)
      console.lines.length shouldEqual lines.count {
        case _: LogLine.Fatal => true
        case _: LogLine.Error => true
        case _: LogLine.Warn => true
        case _ => false
      }
    }
  }

  test("LoggerFunction.log with level Info") {
    forAll { lines: List[LogLine] ⇒
      val console = MutableConsole(LogLevel.InfoLevel)
      lines.foreach(console.logger.log)
      console.lines.length shouldEqual lines.count {
        case _: LogLine.Debug => false
        case _: LogLine.Trace => false
        case _ => true
      }
    }
  }

  test("LoggerFunction.log with level Debug") {
    forAll { lines: List[LogLine] ⇒
      val console = MutableConsole(LogLevel.DebugLevel)
      lines.foreach(console.logger.log)
      console.lines.length shouldEqual lines.count {
        case _: LogLine.Trace => false
        case _ => true
      }
    }
  }

  test("LoggerFunction.log with level Off") {
    forAll { lines: List[LogLine] ⇒
      val console = MutableConsole(LogLevel.OffLevel)
      lines.foreach(console.logger.log)
      console.lines.length shouldEqual 0
    }
  }
}
