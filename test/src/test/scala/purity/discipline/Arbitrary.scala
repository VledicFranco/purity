package purity.discipline

import org.scalacheck.{Arbitrary, Gen}
import purity.logging.LogLine
import purity.script.ScriptT

object arbitrary {

  implicit def arbitraryScriptT[F[+_], D, E, A](implicit F: Arbitrary[F[(List[LogLine], Either[E, A])]]): Arbitrary[ScriptT[F, D, E, A]] =
    Arbitrary(F.arbitrary.map(x => ScriptT((_: D) => x)))

  implicit def arbitraryLogLine: Arbitrary[LogLine] = Arbitrary(logLineGen)

  def logLineGen: Gen[LogLine] =
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
}
