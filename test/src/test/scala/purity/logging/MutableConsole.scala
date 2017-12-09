package purity.logging

import purity.Proposition
import purity.Truth.{False, True}

import scala.collection.mutable.MutableList

case class MutableConsole(level: LogLevel) {
  val lines: MutableList[LogLine] = MutableList.empty
  val logger = LoggerFunction(line => lines += line, level)
}

object MutableConsole {

  def hasAmountOfLines(n: Int): Proposition[String, MutableConsole] =
    Proposition { console =>
      val lines = console.lines.length
      if (lines == n) True
      else False(s"The mutable console should have $n lines, but contains $lines:${console.lines.mkString("\n","\n","\n")}")
    }
}


