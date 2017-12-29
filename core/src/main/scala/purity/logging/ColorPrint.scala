package purity.logging

import cats.effect.IO
import purity.logging.LogLine._
import java.time.LocalDateTime

trait ColorPrint extends Logger[IO] {

  protected def prSource: Boolean

  protected def prTime: Boolean

  override def logEffect(line: LogLine): IO[Unit] = line match {
    case line: Fatal=>
      pr(Console.BOLD + Console.RED, "fatal", line.message, line.source, line.e)
    case line: Error=>
      pr(Console.RED, "error", line.message, line.source, line.e)
    case line: Warn=>
      pr(Console.YELLOW, "warn", line.message, line.source, line.e)
    case line: Info=>
      pr(Console.GREEN, "info", line.message, line.source, line.e)
    case line: Debug=>
      pr(Console.MAGENTA, "debug", line.message, line.source, line.e)
    case line: Trace=>
      pr(Console.CYAN, "trace", line.message, line.source, line.e)
  }

  private def pr(color: String, level: String, message: String, source: Source, e: Option[Throwable]): IO[Unit] =
    IO {
      val reset = Console.RESET
      val lvl = s"[$color$level$reset]"
      val tm = if(prTime) s" [${LocalDateTime.now().toString}]" else ""
      val sr = if(prSource) s" $source\n$lvl" else ""
      println(s"$lvl$tm$sr $message")
      e.foreach(_.printStackTrace())
    }
}

object ColorPrint {

  def apply(l: LogLevel, printSource: Boolean = false, printTime: Boolean = false): ColorPrint =
    new ColorPrint {
      override val prSource: Boolean = printSource
      override val prTime: Boolean = printTime
      override val level: LogLevel = l
    }
}