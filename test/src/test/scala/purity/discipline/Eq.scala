package purity.discipline

import cats.kernel._
import org.scalacheck.Arbitrary
import purity.logging.LogLine
import purity.script.ScriptT

object eq {

  implicit def eqForThrowable: Eq[Throwable] = Eq.fromUniversalEquals

  implicit def eqForLogLine: Eq[LogLine] = Eq.fromUniversalEquals

  implicit def eqForScriptT[F[+_], D, E, A](implicit Feq: Eq[F[(List[LogLine], Either[E, A])]], arbD: Arbitrary[D]): Eq[ScriptT[F, D, E, A]] =
    (x: ScriptT[F, D, E, A], y: ScriptT[F, D, E, A]) => {
      val dependencies = arbD.arbitrary.sample.get
      Feq.eqv(x.definition(dependencies), y.definition(dependencies))
    }
}
