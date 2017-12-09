package purity.logging

import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.{Arbitrary, Gen}

object Generators {

  val logLineGen: Gen[LogLine] =
    for {
      message <- arbitrary[String]
      level <- Gen.choose(1, 6)
    } yield level match {
      case 1 => LogLine.trace(message)
      case 2 => LogLine.debug(message)
      case 3 => LogLine.info(message)
      case 4 => LogLine.warn(message)
      case 5 => LogLine.error(message)
      case _ => LogLine.fatal(message)
    }

  implicit val arbitraryLogLine: Arbitrary[LogLine] = Arbitrary(logLineGen)
}
