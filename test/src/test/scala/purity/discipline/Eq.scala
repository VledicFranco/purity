package purity.discipline

import cats.effect.laws.util.TestInstances
import cats.kernel._
import cats.kernel.laws._
import org.scalacheck.Arbitrary
import purity.logging.LogLine
//import purity.{Predicate, PropositionT, Truth}

object eq extends TestInstances {

  implicit def eqForThrowable: Eq[Throwable] = Eq.fromUniversalEquals

  implicit def eqForLogLine: Eq[LogLine] = Eq.fromUniversalEquals

  /*
  implicit def eqForTruth[E]: Eq[Truth[E]] =
    Eq.fromUniversalEquals

  implicit def eqForProposition[E, A](implicit A: Arbitrary[A]): Eq[Proposition[E, A]] =
    new Eq[Proposition[E, A]] {

        val sampleCnt: Int = 50

        def eqv(f: Proposition[E, A], g: Proposition[E, A]): Boolean = {
          val samples = List.fill(sampleCnt)(A.arbitrary.sample).collect{
            case Some(a) => a
            case None => sys.error("Could not generate arbitrary values to compare two propositions")
          }
          samples.forall(s => Eq[Truth[E]].eqv(f.check(s), g.check(s)))
        }
    }

  implicit def eqForPredicate[A](implicit A: Arbitrary[A]): Eq[Predicate[A]] =
    new Eq[Predicate[A]] {

      val sampleCnt: Int = 50

      def eqv(f: Predicate[A], g: Predicate[A]): Boolean = {
        val samples = List.fill(sampleCnt)(A.arbitrary.sample).collect{
          case Some(a) => a
          case None => sys.error("Could not generate arbitrary values to compare two predicates")
        }
        samples.forall(s => f.check(s) == g.check(s))
      }
    }
    */
}
