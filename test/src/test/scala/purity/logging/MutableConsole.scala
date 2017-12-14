package purity.logging

import purity.Proposition
import purity.Truth.{False, True}
import purity.logging.MutableConsole.ConsoleBuffer

import scala.collection.mutable

case class MutableConsole(level: LogLevel, buffer: ConsoleBuffer = MutableConsole.emptyBuffer) {

  val logger = LoggerFunction(line => buffer += line, level)

  def flush(): List[LogLine] = {
    val cpy = buffer.toList
    buffer.clear()
    cpy
  }
}

object MutableConsole {

  type ConsoleBuffer = mutable.MutableList[LogLine]

  def emptyBuffer: ConsoleBuffer = mutable.MutableList.empty[LogLine]

  def hasAmountOfLines(n: Int): Proposition[String, MutableConsole] =
    Proposition { console =>
      val lines = console.buffer.length
      if (lines == n) True
      else False(s"The mutable console should have $n lines, but contains $lines:${console.buffer.mkString("\n","\n","\n")}")
    }
}


