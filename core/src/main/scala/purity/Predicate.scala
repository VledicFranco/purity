package purity

import cats.Contravariant
import cats.data.NonEmptyList
import purity.Truth.{False, True}

trait Predicate[-A] {

  def check(a: A): Boolean

  def contramap[B](f: B => A): Predicate[B] =
    a => (f andThen check)(a)

  def and[AA <: A](that: Predicate[AA]): Predicate[AA] =
    a => check(a) && that.check(a)

  def or[AA <: A](that: Predicate[AA]): Predicate[AA] =
    a => check(a) || that.check(a)

  def toProposition[E](e: E, es: E*): Proposition[E, A] =
    this.toProposition(NonEmptyList(e, es.toList))

  def toProposition[E](es: NonEmptyList[E]): Proposition[E, A] =
    a => if(check(a)) True else False(es)
}

object Predicate extends PredicateFunctions with PredicateInstances {

  def apply[A](f: A => Boolean): Predicate[A] = a => f(a)
}

private[purity] trait PredicateFunctions {

  def tautology[A]: Predicate[A] = _ => true

  def contradiction[A]: Predicate[A] = _ => false
}

private[purity] trait PredicateInstances {

  implicit def stdContravariant: Contravariant[Predicate] =
    contravariantAnd

  def contravariantAnd: Contravariant[Predicate] =
    new Contravariant[Predicate] {
      override def contramap[A, B](fa: Predicate[A])(f: (B) => A): Predicate[B] =
        fa.contramap(f)
    }

  def contravariantOr: Contravariant[Predicate] =
    new Contravariant[Predicate] {
      override def contramap[A, B](fa: Predicate[A])(f: (B) => A): Predicate[B] =
        fa.contramap(f)
    }
}
