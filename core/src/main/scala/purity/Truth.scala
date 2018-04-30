package purity

import scalaz.Functor
import matryoshka._
import matryoshka.data.Mu
import matryoshka.implicits._
import purity.Truth._

sealed trait Truth[A] { p =>

  type Self = Mu[Truth]

  def not(implicit ev0: Corecursive.Aux[Self, Truth]): Self =
    Not[Self](p.embed).embed

  def &&(q: Truth[A]): Truth[A] = And(p, q)

  def ||(q: Truth[A]): Truth[A] = Or(p, q)

  def ==>(q: Truth[A]): Truth[A] = not || q

  def xor(q: Truth[A]): Truth[A] = (p || q) && (p && q).not

  def fold[B](f: Truth[A] => B): B = f(p)
}

object Truth extends TruthInstances {

  final case class True[A](tag: Option[String]) extends Truth[A]

  final case class False[A](tag: Option[String]) extends Truth[A]

  final case class Equals[A](p: A, q: A) extends Truth[A]

  final case class Not[A](p: A) extends Truth[A]

  final case class And[A](p: A, q: A) extends Truth[A]

  final case class Or[A](p: A, q: A) extends Truth[A]

  val tracker: Algebra[Truth, (String, Boolean)] =  {
    case True(tag) =>
      (tag.getOrElse("T"), true)

    case False(tag) =>
      (tag.getOrElse("F"), false)

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
}

private[purity] trait TruthInstances {

  implicit val stdFunctorOfTruth: Functor[Truth] =
    new Functor[Truth] {
      override def map[A, B](fa: Truth[A])(f: A => B): Truth[B] =
        fa match {
          case True(tag) => True(tag)
          case False(tag) => False(tag)
          case Equals(p, q) => Equals(f(p), f(q))
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
