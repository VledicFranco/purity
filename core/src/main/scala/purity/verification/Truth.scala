package purity.verification

import cats.{Eq, Show}
import matryoshka._
import scalaz.Functor
import purity.verification.Truth._

sealed trait Truth[T]

object Truth extends TruthFunctions with TruthInstances {

  final case class True[T](tag: Option[String]) extends Truth[T]

  final case class False[T](tag: Option[String]) extends Truth[T]

  final case class Equals[T, X](x: () => X, y: () => X, eq: Eq[X], pr: Show[X]) extends Truth[T]

  final case class Not[T](p: T) extends Truth[T]

  final case class And[T](p: T, q: T) extends Truth[T]

  final case class Or[T](p: T, q: T) extends Truth[T]

  val tracker: Algebra[Truth, (String, Boolean)] = {
    case True(tag) =>
      (tag.getOrElse("T"), true)

    case False(tag) =>
      (tag.getOrElse("F"), false)

    case Equals(x, y, eq, pr) =>
      if(eq.eqv(x(), y())) ("T", true)
      else (s"(${pr.show(x())} == ${pr.show(y())})", false)

    case Not((pTag, p)) =>
      if(!p) ("T", true)
      else (s"(!$pTag)", false)

    case And((pTag, p), (qTag, q)) =>
      if (p && q) ("T", true)
      else if (p) (s"(T && $qTag)", false)
      else if (q) (s"($pTag && T)", false)
      else (s"($pTag && $qTag)", false)

    case Or((pTag, p), (qTag, q)) =>
      if (p || q) ("T", true)
      else (s"($pTag || $qTag)", false)
  }

  case class Contradiction(track: String) extends RuntimeException
}

private[purity] trait TruthFunctions { functions =>

  private val ev0 = implicitly[Corecursive.Aux[TruthFix, Truth]]

  def veritas(tag: String): TruthFix =
    ev0.embed(True[TruthFix](Some(tag)))

  def falsum(tag: String): TruthFix =
    ev0.embed(False[TruthFix](Some(tag)))

  def =:=[X](x: => X, y: => X)(implicit ev1: Eq[X], ev2: Show[X]): TruthFix =
    ev0.embed(Equals[TruthFix, X](() => x, () => y, ev1, ev2))

  def not(p: TruthFix): TruthFix =
    ev0.embed(Not[TruthFix](p))

  def &&(p: TruthFix, q: TruthFix): TruthFix =
    ev0.embed(And[TruthFix](p, q))

  def ||(p: TruthFix, q: TruthFix): TruthFix =
    ev0.embed(Or[TruthFix](p, q))

  def ==>(p: TruthFix, q: TruthFix): TruthFix =
    ||(not(p), q)

  def xor(p: TruthFix, q: TruthFix): TruthFix =
    &&(||(p, q), not(&&(p, q)))

  class OpsForTruth(p: TruthFix) {

    def not: TruthFix = functions.not(p)

    def &&(q: TruthFix): TruthFix = functions.&&(p, q)

    def ||(q: TruthFix): TruthFix = functions.||(p, q)

    def ==>(q: TruthFix): TruthFix = functions.==>(p, q)

    def xor(q: TruthFix): TruthFix = functions.xor(p, q)
  }

  class OpsForAny[X](x: => X) {

    def =:=(y: => X)(implicit ev1: Eq[X], ev2: Show[X]): TruthFix =
      functions.=:=[X](x, y)
  }
}

private[purity] trait TruthInstances {

  implicit val stdFunctorOfTruth: Functor[Truth] =
    new Functor[Truth] {
      override def map[A, B](fa: Truth[A])(f: A => B): Truth[B] =
        fa match {
          case True(tag) => True(tag)
          case False(tag) => False(tag)
          case Equals(x, y, eq, pr) => Equals(x, y, eq, pr)
          case Not(p) => Not(f(p))
          case And(p, q) => And(f(p), f(q))
          case Or(p, q) => Or(f(p), f(q))
        }
    }
}
