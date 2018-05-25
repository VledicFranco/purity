package purity.verification

import cats.{Eq, Show}
import matryoshka._
import matryoshka.data.Mu
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

  def isTrue[T](tag: String)(implicit ev0: Corecursive.Aux[T, Truth]): T =
    ev0.embed(True[T](Some(tag)))

  def isFalse[T](tag: String)(implicit ev0: Corecursive.Aux[T, Truth]): T =
    ev0.embed(False[T](Some(tag)))

  def =:=[T, X](x: => X, y: => X)(implicit ev0: Corecursive.Aux[T, Truth], ev1: Eq[X], ev2: Show[X]): T =
    ev0.embed(Equals[T, X](() => x, () => y, ev1, ev2))

  def not[T](p: T)(implicit ev0: Corecursive.Aux[T, Truth]): T =
    ev0.embed(Not[T](p))

  def &&[T](p: T, q: T)(implicit ev0: Corecursive.Aux[T, Truth]): T =
    ev0.embed(And[T](p, q))

  def ||[T](p: T, q: T)(implicit ev0: Corecursive.Aux[T, Truth]): T =
    ev0.embed(Or[T](p, q))

  def ==>[T](p: T, q: T)(implicit ev0: Corecursive.Aux[T, Truth]): T =
    ||(not(p), q)

  def xor[T](p: T, q: T)(implicit ev0: Corecursive.Aux[T, Truth]): T =
    &&(||(p, q), not(&&(p, q)))

  class OpsForTruth[T](p: T)(implicit ev0: Corecursive.Aux[T, Truth]) {

    def not: T = functions.not(p)

    def &&(q: T): T = functions.&&(p, q)

    def ||(q: T): T = functions.||(p, q)

    def ==>(q: T): T = functions.==>(p, q)

    def xor(q: T): T = functions.xor(p, q)
  }

  class OpsForAny[X](x: => X) {

    def =:=(y: => X)(implicit ev1: Eq[X], ev2: Show[X]): Mu[Truth] =
      functions.=:=[Mu[Truth], X](x, y)
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
