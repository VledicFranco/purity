package purity.discipline

import cats.Functor
import cats.kernel._
import cats.kernel.laws._
import cats.laws.discipline.SemigroupalTests.Isomorphisms
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

  implicit def stdIsosForScriptT[F[+_], D, E](implicit F: Functor[F]): Isomorphisms[ScriptT[F, D, E, ?]] =
    new Isomorphisms[ScriptT[F, D, E, ?]] {
      def associativity[A, B, C](fs: (ScriptT[F, D, E, (A, (B, C))], ScriptT[F, D, E, ((A, B), C)])): IsEq[ScriptT[F, D, E, (A, B, C)]] =
        fs._1.map { case (a, (b, c)) => (a, b, c) } <-> fs._2.map { case ((a, b), c) => (a, b, c) }

      def leftIdentity[A](fs: (ScriptT[F, D, E, (Unit, A)], ScriptT[F, D, E, A])): IsEq[ScriptT[F, D, E, A]] =
        fs._1.map { case (_, a) => a } <-> fs._2

      def rightIdentity[A](fs: (ScriptT[F, D, E, (A, Unit)], ScriptT[F, D, E, A])): IsEq[ScriptT[F, D, E, A]] =
        fs._1.map { case (a, _) => a } <-> fs._2
    }

}
