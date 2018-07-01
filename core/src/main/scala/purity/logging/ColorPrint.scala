package purity.logging

import cats.effect.Sync
import purity.logging.LogLine._
import java.time.LocalDateTime
import scala.{Console => Color}

object ColorPrint {

  def apply[F[_]: Sync](level: LogLevel, printSource: Boolean = false, printTime: Boolean = false): Logger[F] =
    Logger[F](level, line => logEffect(line, printSource, printTime))

  def logEffect[F[_]: Sync](line: LogLine, printSource: Boolean, printTime: Boolean): F[Unit] = line match {
    case line: Fatal =>
      pr(Color.BOLD + Color.RED, "fatal", line.message, line.source, line.e, printSource, printTime)
    case line: Error =>
      pr(Color.RED, "error", line.message, line.source, line.e, printSource, printTime)
    case line: Warn =>
      pr(Color.YELLOW, "warn", line.message, line.source, line.e, printSource, printTime)
    case line: Info =>
      pr(Color.GREEN, "info", line.message, line.source, line.e, printSource, printTime)
    case line: Debug =>
      pr(Color.MAGENTA, "debug", line.message, line.source, line.e, printSource, printTime)
    case line: Trace =>
      pr(Color.CYAN, "trace", line.message, line.source, line.e, printSource, printTime)
  }

  private def pr[F[_]](
      color: String,
      level: String,
      message: String,
      source: Source,
      e: Option[Throwable],
      printSource: Boolean,
      printTime: Boolean)(
      implicit
      F: Sync[F]
  ): F[Unit] =
    F.delay {
      val reset = Color.RESET
      val lvl = s"[$color$level$reset]"
      val tm = if(printTime) s" [${LocalDateTime.now().toString}]" else ""
      val sr = if(printSource) s" $source\n$lvl" else ""
      println(s"$lvl$tm$sr $message")
      e.foreach(_.printStackTrace())
    }
}