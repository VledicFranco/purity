package purity.logging

import cats.effect.Sync
import cats.implicits._
import purity.logging.Console.ConsoleBuffer

import scala.collection.mutable

case class Console[F[_]](level: LogLevel, buffer: ConsoleBuffer = Console.emptyBuffer)(implicit F: Sync[F]) { self =>

  val logger: Logger[F] = 
    Logger[F](
      level,
      line => F.delay(buffer += line)
    )

  def log(line: LogLine): F[Unit] = logger.log(line)

  def log(lines: List[LogLine]): F[Unit] = lines.traverse(logger.log).void

  def flush: F[List[LogLine]] = {
    for {
      cpy <- F.delay(buffer.toList)
      _ <- F.delay(buffer.clear())
    } yield cpy
  }
}

object Console {

  type ConsoleBuffer = mutable.MutableList[LogLine]

  def emptyBuffer: ConsoleBuffer = mutable.MutableList.empty[LogLine]
}


