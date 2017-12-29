package purity.logging

import cats.Order
import purity.logging.LogLevel._
import purity.logging.LogLine.Source

sealed abstract class LogLine(val line: String, val level: LogLevel, val source: Source, val error: Option[Throwable])

object LogLine {

  case class Source(file: sourcecode.File, line: sourcecode.Line) {

    override def toString: String = file.value+": "+line.value
  }

  case class Fatal(message: String, e: Option[Throwable] = None)
                  (implicit fl: sourcecode.File, cl: sourcecode.Line)
    extends LogLine(message, FatalLevel, Source(fl, cl), e)

  case class Error(message: String, e: Option[Throwable] = None)
                  (implicit fl: sourcecode.File, cl: sourcecode.Line)
    extends LogLine(message, ErrorLevel, Source(fl, cl), e)

  case class Warn(message: String, e: Option[Throwable] = None)
                 (implicit fl: sourcecode.File, cl: sourcecode.Line)
    extends LogLine(message, WarnLevel, Source(fl, cl), e)

  case class Info(message: String, e: Option[Throwable] = None)
                 (implicit fl: sourcecode.File, cl: sourcecode.Line)
    extends LogLine(message, InfoLevel, Source(fl, cl), e)

  case class Debug(message: String, e: Option[Throwable] = None)
                  (implicit fl: sourcecode.File, cl: sourcecode.Line)
    extends LogLine(message, DebugLevel, Source(fl, cl), e)

  case class Trace(message: String, e: Option[Throwable] = None)
                  (implicit fl: sourcecode.File, cl: sourcecode.Line)
    extends LogLine(message, AllLevel, Source(fl, cl), e)

  def debug(message: String)(implicit fl: sourcecode.File, cl: sourcecode.Line): LogLine =
    Debug(message, None)(fl, cl)

  def debug(e: Throwable)(implicit fl: sourcecode.File, cl: sourcecode.Line): LogLine =
    Debug(e.getMessage, Some(e))(fl, cl)

  def debug(message: String, e: Throwable)(implicit fl: sourcecode.File, cl: sourcecode.Line): LogLine =
    Debug(message, Some(e))(fl, cl)

  def error(message: String)(implicit fl: sourcecode.File, cl: sourcecode.Line): LogLine =
    Error(message, None)(fl, cl)

  def error(e: Throwable)(implicit fl: sourcecode.File, cl: sourcecode.Line): LogLine =
    Error(e.getMessage, Some(e))(fl, cl)

  def error(message: String, e: Throwable)(implicit fl: sourcecode.File, cl: sourcecode.Line): LogLine =
    Error(message, Some(e))(fl, cl)

  def fatal(message: String)(implicit fl: sourcecode.File, cl: sourcecode.Line): LogLine =
    Fatal(message, None)(fl, cl)

  def fatal(e: Throwable)(implicit fl: sourcecode.File, cl: sourcecode.Line): LogLine =
    Fatal(e.getMessage, Some(e))(fl, cl)

  def fatal(message: String, e: Throwable)(implicit fl: sourcecode.File, cl: sourcecode.Line): LogLine =
    Fatal(message, Some(e))(fl, cl)

  def info(message: String)(implicit fl: sourcecode.File, cl: sourcecode.Line): LogLine =
    Info(message, None)(fl, cl)

  def info(e: Throwable)(implicit fl: sourcecode.File, cl: sourcecode.Line): LogLine =
    Info(e.getMessage, Some(e))(fl, cl)

  def info(message: String, e: Throwable)(implicit fl: sourcecode.File, cl: sourcecode.Line): LogLine =
    Info(message, Some(e))(fl, cl)

  def trace(message: String)(implicit fl: sourcecode.File, cl: sourcecode.Line): LogLine =
    Trace(message, None)(fl, cl)

  def trace(e: Throwable)(implicit fl: sourcecode.File, cl: sourcecode.Line): LogLine =
    Trace(e.getMessage, Some(e))(fl, cl)

  def trace(message: String, e: Throwable)(implicit fl: sourcecode.File, cl: sourcecode.Line): LogLine =
    Trace(message, Some(e))(fl, cl)

  def warn(message: String)(implicit fl: sourcecode.File, cl: sourcecode.Line): LogLine =
    Warn(message, None)(fl, cl)

  def warn(e: Throwable)(implicit fl: sourcecode.File, cl: sourcecode.Line): LogLine =
    Warn(e.getMessage, Some(e))(fl, cl)

  def warn(message: String, e: Throwable)(implicit fl: sourcecode.File, cl: sourcecode.Line): LogLine =
    Warn(message, Some(e))(fl, cl)

  implicit val logLineOrder: Order[LogLine] = Order.by(_.level)

  implicit val logLineOrdering: Ordering[LogLine] = logLineOrder.toOrdering
}

