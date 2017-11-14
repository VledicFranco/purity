package purity.logging

import purity.logging.LogLevel._
import purity.logging.LogLine._

object ColorPrint {

  def apply(level: LogLevel = AllLevel): LoggerFunction = LoggerFunction({
    case Fatal(message, e) ⇒
      println(Console.BOLD + Console.RED + "[DEBUG] " + message + Console.RESET)
      e.foreach { e ⇒
        println(e.getMessage)
        e.printStackTrace()
      }
    case Error(message, e) ⇒
      println(Console.RED + "[DEBUG] " + message + Console.RESET)
      e.foreach { e ⇒
        println(e.getMessage)
        e.printStackTrace()
      }
    case Warn(message, e) ⇒
      println(Console.YELLOW + "[WARN] " + message + Console.RESET)
      e.foreach { e ⇒
        println(e.getMessage)
        e.printStackTrace()
      }
    case Info(message, e) ⇒
      println(Console.GREEN + "[INFO] " + message + Console.RESET)
      e.foreach { e ⇒
        println(e.getMessage)
        e.printStackTrace()
      }
    case Debug(message, e) ⇒
      println(Console.MAGENTA + "[DEBUG] " + message + Console.RESET)
      e.foreach { e ⇒
        println(e.getMessage)
        e.printStackTrace()
      }
    case Trace(message, e) ⇒
      println(Console.CYAN + "[TRACE] " + message + Console.RESET)
      e.foreach { e ⇒
        println(e.getMessage)
        e.printStackTrace()
      }
  }, level)
}