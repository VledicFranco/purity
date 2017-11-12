package purity

import cats.{ Functor, MonoidK, SemigroupK }
import cats.data.NonEmptyList
import purity.Truth.{ True, False }

/**
 * Coproduct that represents a logical truth. Truth is either True or False(e)
 *
 * One may also view the Truth data type as an inverse Option, or a cats Validated with the Valid type parameter
 * set to Unit.
 *
 * Contains combinators map, not, &&, ||
 *
 * @tparam E type which encodes possible failures (The False data type will contain a non empty list of these)
 */
sealed trait Truth[+E] { p ⇒

  def isTrue: Boolean

  def isFalse: Boolean = !isTrue

  def counterExamples: NonEmptyList[E]

  def map[B](f: E ⇒ B): Truth[B] =
    if (isFalse) False(counterExamples.map(f))
    else True

  def not[EE >: E](e: EE): Truth[EE] =
    if (isTrue) Truth.isFalse(e)
    else True

  def &&[EE >: E](q: Truth[EE]): Truth[EE] =
    if (isFalse && q.isTrue) p
    else if (isTrue && q.isTrue) True
    else if (isTrue && q.isFalse) q
    else False(counterExamples concatNel q.counterExamples)

  def ||[EE >: E](q: Truth[EE]): Truth[EE] =
    if (isFalse && q.isFalse) False(counterExamples concatNel q.counterExamples)
    else True

  def ifWhenFalse(f: NonEmptyList[E] ⇒ Boolean): Boolean =
    isFalse && f(counterExamples)

  def fold[A](ifFalse: NonEmptyList[E] ⇒ A)(ifTrue: ⇒ A): A =
    if (isTrue) ifTrue else ifFalse(counterExamples)

  def counterExamplesSet[EE >: E]: Set[EE] = counterExamples.toList.toSet
}

object Truth extends TruthFunctions with TruthInstances {

  case object True extends Truth[Nothing] {
    def isTrue: Boolean = true
    def counterExamples: NonEmptyList[Nothing] = throw new NoSuchElementException("Sound.invalidations")
  }

  final case class False[E](e: NonEmptyList[E]) extends Truth[E] {
    override def isTrue: Boolean = false
    def counterExamples: NonEmptyList[E] = e
  }

  object False {
    def apply[E](e: E, es: E*): Truth[E] = new False[E](NonEmptyList(e, es.toList))
  }
}

private[purity] trait TruthFunctions {

  def isFalse[E](e: E): Truth[E] = False(NonEmptyList.one(e))
}

private[purity] trait TruthInstances {

  implicit def stdFunctorForProposition: Functor[Truth] =
    new Functor[Truth] {
      override def map[A, B](fa: Truth[A])(f: (A) ⇒ B): Truth[B] = fa.map(f)
    }

  /** Monoid using the && combinator */
  implicit def stdMonoidForProposition: MonoidK[Truth] =
    new MonoidK[Truth] {
      override def empty[A]: Truth[A] = True
      override def combineK[A](x: Truth[A], y: Truth[A]): Truth[A] = x && y
    }

  /** Semigroup using the || combinator */
  def orSemigroupForProposition: SemigroupK[Truth] =
    new SemigroupK[Truth] {
      override def combineK[A](x: Truth[A], y: Truth[A]): Truth[A] = x || y
    }
}
