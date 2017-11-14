package purity.logging

import cats.implicits._
import purity.logging.LogLevel._
import purity.logging.LogLine._

case class LoggerFunction(f: LogLine ⇒ Unit, level: LogLevel) {

  def log(line: LogLine): Unit = if (line.level >= level) f(line)

  def debug(message: String): Unit = log(Debug(message, None))

  def debug(e: Throwable): Unit = log(Debug(e.getMessage, Some(e)))

  def debug(message: String, e: Throwable): Unit = log(Debug(message, Some(e)))

  def error(message: String): Unit = log(Error(message, None))

  def error(e: Throwable): Unit = log(Error(e.getMessage, Some(e)))

  def error(message: String, e: Throwable): Unit = log(Error(message, Some(e)))

  def fatal(message: String): Unit = log(Fatal(message, None))

  def fatal(e: Throwable): Unit = log(Fatal(e.getMessage, Some(e)))

  def fatal(message: String, e: Throwable): Unit = log(Fatal(message, Some(e)))

  def info(message: String): Unit = log(Info(message, None))

  def info(e: Throwable): Unit = log(Info(e.getMessage, Some(e)))

  def info(message: String, e: Throwable): Unit = log(Info(message, Some(e)))

  def trace(message: String): Unit = log(Trace(message, None))

  def trace(e: Throwable): Unit = log(Trace(e.getMessage, Some(e)))

  def trace(message: String, e: Throwable): Unit = log(Trace(message, Some(e)))

  def warn(message: String): Unit = log(Warn(message, None))

  def warn(e: Throwable): Unit = log(Warn(e.getMessage, Some(e)))

  def warn(message: String, e: Throwable): Unit = log(Warn(message, Some(e)))
}
