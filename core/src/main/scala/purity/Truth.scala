package purity

import cats.{Eq, Show}
import scalaz.Functor
import matryoshka._
import matryoshka.implicits._
import purity.Truth._

sealed trait Truth[T]

object Truth extends TruthFunctions with TruthInstances {

  final case class True[T](tag: Option[String]) extends Truth[T]

  final case class False[T](tag: Option[String]) extends Truth[T]

  final case class Equals[T, X](x: () => X, y: () => X, eq: Eq[X], pr: Show[X]) extends Truth[T]

  final case class Not[T](p: T) extends Truth[T]

  final case class And[T](p: T, q: T) extends Truth[T]

  final case class Or[T](p: T, q: T) extends Truth[T]

  val tracker: Algebra[Truth, (String, Boolean)] =  {
    case True(tag) =>
      (tag.getOrElse("T"), true)

    case False(tag) =>
      (tag.getOrElse("F"), false)

    case Equals(x, y, eq, pr) =>
      if(eq.eqv(x(), y())) ("T", true)
      else (s"(${pr.show(x())} == ${pr.show(y())} :: false)", false)

    case Not((pTag, p)) =>
      if(!p) ("T", true)
      else (s"(!$pTag :: false)", false)

    case And((pTag, p), (qTag, q)) =>
      if (p && q) ("T", true)
      else if (p) (s"(true && $qTag :: false)", false)
      else if (q) (s"($pTag :: false && true)", false)
      else (s"($pTag :: false && $qTag :: false)", false)

    case Or((pTag, p), (qTag, q)) =>
      if (p || q) ("T", true)
      else (s"($pTag :: false || $qTag :: false)", false)
  }
}

private[purity] trait TruthFunctions {

  def =:=[T, X](x: => X, y: => X)(implicit ev0: Corecursive.Aux[T, Truth], ev1: Eq[X], ev2: Show[X]): T =
    ev0.embed(Equals[T, X](() => x, () => y, ev1, ev2))

  def not[T](p: T)(implicit ev0: Corecursive.Aux[T, Truth]): T =
    ev0.embed(Not[T](p))

  def &&[T](q: T, p: T)(implicit ev0: Corecursive.Aux[T, Truth]): T =
    ev0.embed(And[T](p, q))

  /*
  def ||[T](q: Truth[A]): Truth[A] = Or(p, q)

  def ==>[T](q: Truth[A]): Truth[A] = not || q

  def xor[T](q: Truth[A]): Truth[A] = (p || q) && (p && q).not
  */
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
  /** Monoid using the && combinator */
  /*
  implicit def stdMonoidForProposition[A]: Monoid[Truth[A]] =
    new Monoid[Truth[A]] {
      override def empty: Truth[A] = True
      override def combine(x: Truth[A], y: Truth[A]): Truth[A] = x && y
    }
  */

  /** Semigroup using the || combinator */
  /*
  def orSemigroupForProposition: Semigroup[Truth[A]] =
    new Semigroup[Truth[A]] {
      override def combine(x: Truth[A], y: Truth[A]): Truth[A] = x || y
    }
  */
}

trait TruthSyntaxForTruth {

}