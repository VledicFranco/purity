package purity.discipline

import cats.effect.IO
import org.scalacheck.{Arbitrary, Gen}
import purity.logging.{ColorPrint, LogLevel, LogLine, Logger}

object arbitrary {

  implicit def arbitraryLogLine: Arbitrary[LogLine] =
    Arbitrary(
      for {
        message <- Arbitrary.arbitrary[String]
        level <- Gen.choose(1, 6)
      } yield level match {
        case 1 => LogLine.trace(message)
        case 2 => LogLine.debug(message)
        case 3 => LogLine.info(message)
        case 4 => LogLine.warn(message)
        case 5 => LogLine.error(message)
        case _ => LogLine.fatal(message)
      }
    )

  implicit def arbitraryLogLevel: Arbitrary[LogLevel] =
    Arbitrary(Gen.oneOf(
      LogLevel.AllLevel,
      LogLevel.DebugLevel,
      LogLevel.ErrorLevel,
      LogLevel.FatalLevel,
      LogLevel.InfoLevel,
      LogLevel.WarnLevel,
      LogLevel.OffLevel,
    ))

  implicit def arbitraryLogger: Arbitrary[Logger[IO]] =
    Arbitrary(
      for {
        level <- Arbitrary.arbitrary[LogLevel]
        logger <- Gen.oneOf(Logger.VoidLogs[IO], ColorPrint[IO](level))
      } yield logger
    )
}
