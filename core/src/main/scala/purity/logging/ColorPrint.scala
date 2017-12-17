package purity.logging

import purity.logging.LogLevel._
import purity.logging.LogLine._

object ColorPrint {

  def apply(level: LogLevel = AllLevel): LoggerFunction = LoggerFunction({
    case Fatal(message, e) =>
      println(Console.BOLD + Console.RED + "[FATAL] " + message)
      e.foreach(_.printStackTrace())
      print(Console.RESET)
    case Error(message, e) =>
      println(Console.RED + "[ERROR] " + message)
      e.foreach(_.printStackTrace())
      print(Console.RESET)
    case Warn(message, e) =>
      println(Console.YELLOW + "[WARN] " + message)
      e.foreach(_.printStackTrace())
      print(Console.RESET)
    case Info(message, e) =>
      println(Console.GREEN + "[INFO] " + message)
      e.foreach(_.printStackTrace())
      print(Console.RESET)
    case Debug(message, e) =>
      println(Console.MAGENTA + "[DEBUG] " + message)
      e.foreach(_.printStackTrace())
      print(Console.RESET)
    case Trace(message, e) =>
      println(Console.CYAN + "[TRACE] " + message)
      e.foreach(_.printStackTrace())
      print(Console.RESET)
  }, level)
}