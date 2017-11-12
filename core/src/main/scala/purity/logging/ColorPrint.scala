package purity.logging

import purity.logging.LogLevel._
import purity.logging.LogLine._

object ColorPrint {

  def apply(level: LogLevel = AllLevel): LoggerFunction = LoggerFunction({
    case Debug(message, e) ⇒
      println(Console.MAGENTA + message + Console.RESET)
      e.foreach { e ⇒
        println(e.getMessage)
        e.printStackTrace()
      }
    case Error(message, e) ⇒
      println(Console.RED + message + Console.RESET)
      e.foreach { e ⇒
        println(e.getMessage)
        e.printStackTrace()
      }
    case Fatal(message, e) ⇒
      println(Console.BOLD + Console.RED + message + Console.RESET)
      e.foreach { e ⇒
        println(e.getMessage)
        e.printStackTrace()
      }
    case Info(message, e) ⇒
      println(Console.GREEN + message + Console.RESET)
      e.foreach { e ⇒
        println(e.getMessage)
        e.printStackTrace()
      }
    case Off(message, e) ⇒
      println(Console.WHITE + message + Console.RESET)
      e.foreach { e ⇒
        println(e.getMessage)
        e.printStackTrace()
      }
    case Trace(message, e) ⇒
      println(Console.CYAN + message + Console.RESET)
      e.foreach { e ⇒
        println(e.getMessage)
        e.printStackTrace()
      }
    case Warn(message, e) ⇒
      println(Console.YELLOW + message + Console.RESET)
      e.foreach { e ⇒
        println(e.getMessage)
        e.printStackTrace()
      }
  }, level)
}