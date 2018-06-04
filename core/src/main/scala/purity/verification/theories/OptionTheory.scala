package purity.verification.theories

import cats.Applicative
import purity.verification.PropositionalContext
import purity.verification.Truth.falsum

class OptionTheory[F[_]] extends PropositionalContext[F] {

  def isDefined[A](implicit F: Applicative[F]): Proposition[Option[A]] =
    a => cond(a.isDefined) {
      s"$a is defined"
    } {
      s"$a is undefined"
    }

  def isDefinedAndHolds[A](p: Proposition[A])(implicit F: Applicative[F]): Proposition[Option[A]] =
    proposition[Option[A]] {
      case Some(a) => p.check(a)
      case None => F.pure(falsum(s"unexpected None"))
    }

  def isEmpty[A](implicit F: Applicative[F]): Proposition[Option[A]] =
    a => cond(a.isEmpty) {
      s"$a is empty"
    } {
      s"$a is defined"
    }
}
