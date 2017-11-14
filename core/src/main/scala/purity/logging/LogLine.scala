package purity.logging

import cats.Order
import purity.logging.LogLevel._

sealed abstract class LogLine(val line: String, val level: LogLevel, val error: Option[Throwable])

object LogLine {

  case class Fatal(message: String, e: Option[Throwable] = None) extends LogLine(message, FatalLevel, e)

  case class Error(message: String, e: Option[Throwable] = None) extends LogLine(message, ErrorLevel, e)

  case class Warn(message: String, e: Option[Throwable] = None) extends LogLine(message, WarnLevel, e)

  case class Info(message: String, e: Option[Throwable] = None) extends LogLine(message, InfoLevel, e)

  case class Debug(message: String, e: Option[Throwable] = None) extends LogLine(message, DebugLevel, e)

  case class Trace(message: String, e: Option[Throwable] = None) extends LogLine(message, AllLevel, e)

  def debug(message: String): LogLine = Debug(message, None)

  def debug(e: Throwable): LogLine = Debug(e.getMessage, Some(e))

  def debug(message: String, e: Throwable): LogLine = Debug(message, Some(e))

  def error(message: String): LogLine = Error(message, None)

  def error(e: Throwable): LogLine = Error(e.getMessage, Some(e))

  def error(message: String, e: Throwable): LogLine = Error(message, Some(e))

  def fatal(message: String): LogLine = Fatal(message, None)

  def fatal(e: Throwable): LogLine = Fatal(e.getMessage, Some(e))

  def fatal(message: String, e: Throwable): LogLine = Fatal(message, Some(e))

  def info(message: String): LogLine = Info(message, None)

  def info(e: Throwable): LogLine = Info(e.getMessage, Some(e))

  def info(message: String, e: Throwable): LogLine = Info(message, Some(e))

  def trace(message: String): LogLine = Trace(message, None)

  def trace(e: Throwable): LogLine = Trace(e.getMessage, Some(e))

  def trace(message: String, e: Throwable): LogLine = Trace(message, Some(e))

  def warn(message: String): LogLine = Warn(message, None)

  def warn(e: Throwable): LogLine = Warn(e.getMessage, Some(e))

  def warn(message: String, e: Throwable): LogLine = Warn(message, Some(e))

  implicit val logLineOrder: Order[LogLine] = Order.by(_.level)

  implicit val logLineOrdering: Ordering[LogLine] = logLineOrder.toOrdering
}

