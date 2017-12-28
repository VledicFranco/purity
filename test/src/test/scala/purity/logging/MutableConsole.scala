package purity.logging

import cats.implicits._
import cats.effect.IO
import purity.Proposition
import purity.Truth.{False, True}
import purity.logging.MutableConsole.ConsoleBuffer

import scala.collection.mutable

case class MutableConsole(level: LogLevel, buffer: ConsoleBuffer = MutableConsole.emptyBuffer) { self =>

  val logger: Logger[IO] = Logger[IO](line => IO(buffer += line), level)

  val loggerContainer: LoggerContainer[IO] =
    new LoggerContainer[IO] {
      override def logger: Logger[IO] = self.logger
    }

  def log(line: LogLine): IO[Unit] = logger.log(line)

  def log(lines: List[LogLine]): IO[Unit] = lines.traverse(logger.log).void

  def flush(loglines: IO[Unit]): List[LogLine] = {
    val io: IO[List[LogLine]] = for {
      _ <- loglines
      cpy <- IO(buffer.toList)
      _ <- IO(buffer.clear())
    } yield cpy
    io.unsafeRunSync()
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


