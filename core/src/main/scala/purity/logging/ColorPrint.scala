package purity.logging

import cats.effect.IO
import purity.logging.LogLine._

trait ColorPrint extends Logger[IO] {

  override def logEffect(line: LogLine): IO[Unit] = line match {
    case Fatal(message, e) =>
      pr(Console.BOLD + Console.RED + "[FATAL] " + message, e)
    case Error(message, e) =>
      pr(Console.RED + "[ERROR] " + message, e)
    case Warn(message, e) =>
      pr(Console.YELLOW + "[WARN] " + message, e)
    case Info(message, e) =>
      pr(Console.GREEN + "[INFO] " + message, e)
    case Debug(message, e) =>
      pr(Console.MAGENTA + "[DEBUG] " + message, e)
    case Trace(message, e) =>
      pr(Console.CYAN + "[TRACE] " + message, e)
  }

  private def pr(message: String, e: Option[Throwable]): IO[Unit] =
    IO {
      println(message)
      e.foreach(_.printStackTrace())
      print(Console.RESET)
    }
}

object ColorPrint {

  def apply(l: LogLevel): ColorPrint =
    new ColorPrint {
      override val level: LogLevel = l
    }
}