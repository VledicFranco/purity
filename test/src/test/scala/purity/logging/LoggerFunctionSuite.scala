package purity.logging

import purity.PuritySuite
import cats.kernel.Monoid
import purity.discipline.arbitrary._

class LoggerFunctionSuite extends PuritySuite {

  test("LoggerFunction.log with level All") {
    forAll { lines: List[LogLine] =>
      val console = MutableConsole(LogLevel.AllLevel)
      lines.foreach(console.logger.log)
      console.buffer.length shouldEqual lines.length
    }
  }

  test("LoggerFunction.log with level Fatal") {
    forAll { lines: List[LogLine] =>
      val console = MutableConsole(LogLevel.FatalLevel)
      lines.foreach(console.logger.log)
      console.buffer.length shouldEqual lines.count {
        case _: LogLine.Fatal => true
        case _ => false
      }
    }
  }

  test("LoggerFunction.log with level Error") {
    forAll { lines: List[LogLine] =>
      val console = MutableConsole(LogLevel.ErrorLevel)
      lines.foreach(console.logger.log)
      console.buffer.length shouldEqual lines.count {
        case _: LogLine.Fatal => true
        case _: LogLine.Error => true
        case _ => false
      }
    }
  }

  test("LoggerFunction.log with level Warn") {
    forAll { lines: List[LogLine] =>
      val console = MutableConsole(LogLevel.WarnLevel)
      lines.foreach(console.logger.log)
      console.buffer.length shouldEqual lines.count {
        case _: LogLine.Fatal => true
        case _: LogLine.Error => true
        case _: LogLine.Warn => true
        case _ => false
      }
    }
  }

  test("LoggerFunction.log with level Info") {
    forAll { lines: List[LogLine] =>
      val console = MutableConsole(LogLevel.InfoLevel)
      lines.foreach(console.logger.log)
      console.buffer.length shouldEqual lines.count {
        case _: LogLine.Debug => false
        case _: LogLine.Trace => false
        case _ => true
      }
    }
  }

  test("LoggerFunction.log with level Debug") {
    forAll { lines: List[LogLine] =>
      val console = MutableConsole(LogLevel.DebugLevel)
      lines.foreach(console.logger.log)
      console.buffer.length shouldEqual lines.count {
        case _: LogLine.Trace => false
        case _ => true
      }
    }
  }

  test("LoggerFunction.log with level Off") {
    forAll { lines: List[LogLine] =>
      val console = MutableConsole(LogLevel.OffLevel)
      lines.foreach(console.logger.log)
      console.buffer.length shouldEqual 0
    }
  }

  test("LoggerFunction monoid laws: associativity") {
    forAll { lines: List[LogLine] =>
      val buffer = MutableConsole.emptyBuffer
      val a = MutableConsole(LogLevel.FatalLevel, buffer)
      val b = MutableConsole(LogLevel.ErrorLevel, buffer)
      val c = MutableConsole(LogLevel.DebugLevel, buffer)
      val abxc = (a.logger |+| b.logger) |+| c.logger
      val axbc = a.logger |+| (b.logger |+| c.logger)
      lines.foreach(abxc.log)
      val resultA = a.flush()
      lines.foreach(axbc.log)
      val resultB = a.flush()
      resultA shouldEqual resultB
    }
  }

  test("LoggerFunction monoid laws: left identity") {
    forAll { lines: List[LogLine] =>
      val console = MutableConsole(LogLevel.AllLevel)
      val zero = Monoid[LoggerFunction].empty
      val composed = zero |+| console.logger
      lines.foreach(console.logger.log)
      val resultA = console.flush()
      lines.foreach(console.logger.log)
      val resultB = console.flush()
      resultA shouldEqual resultB
    }
  }

  test("LoggerFunction monoid laws: right identity") {
    forAll { lines: List[LogLine] =>
      val console = MutableConsole(LogLevel.AllLevel)
      val zero = Monoid[LoggerFunction].empty
      val composed = console.logger |+| zero
      lines.foreach(console.logger.log)
      val resultA = console.flush()
      lines.foreach(console.logger.log)
      val resultB = console.flush()
      resultA shouldEqual resultB
    }
  }
}
