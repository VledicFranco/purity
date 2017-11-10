package purity.logging

import cats.Order
import purity.logging.LogLevel._

sealed abstract class LogLine(val line: String, val level: LogLevel, val error: Option[Throwable])

object LogLine {

  case class Debug(message: String, e: Option[Throwable] = None) extends LogLine(message, ErrorLevel, e)

  case class Error(message: String, e: Option[Throwable] = None) extends LogLine(message, ErrorLevel, e)

  case class Fatal(message: String, e: Option[Throwable] = None) extends LogLine(message, FatalLevel, e)

  case class Info(message: String, e: Option[Throwable] = None) extends LogLine(message, InfoLevel, e)

  case class Off(message: String, e: Option[Throwable] = None) extends LogLine(message, OffLevel, e)

  case class Trace(message: String, e: Option[Throwable] = None) extends LogLine(message, TraceLevel, e)

  case class Warn(message: String, e: Option[Throwable] = None) extends LogLine(message, WarnLevel, e)

  def apply(message: String, logLevel: LogLevel, e: Option[Throwable] = None): LogLine =
    logLevel match {
      case AllLevel   ⇒ Info(message, e)
      case DebugLevel ⇒ Debug(message, e)
      case ErrorLevel ⇒ Error(message, e)
      case FatalLevel ⇒ Fatal(message, e)
      case InfoLevel  ⇒ Info(message, e)
      case OffLevel   ⇒ Off(message, e)
      case TraceLevel ⇒ Trace(message, e)
      case WarnLevel  ⇒ Warn(message, e)
    }

  implicit val logLineOrder: Order[LogLine] = Order.by(_.level)

  implicit val logLineOrdering: Ordering[LogLine] = logLineOrder.toOrdering
}

