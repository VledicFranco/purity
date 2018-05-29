package purity.verification

import cats.implicits._
import cats.{Applicative, Eq, Order}
import purity.verification.Truth.{falsum, veritas}

trait Propositions[F[_]] {

  type Proposition[A] = PropositionT[F, A]

  def proposition[A](f: A => F[TruthFix]): Proposition[A] =
    a => f(a)

  def cond[A](p: Boolean)(tagT: => String)(tagF: => String)(implicit F: Applicative[F]): F[TruthFix] =
    F.pure {
      if (p) veritas(tagT)
      else falsum(tagF)
    }

  def equals[A](other: A)(implicit ord: Eq[A], F: Applicative[F]): PropositionT[F, A] =
    a => cond(a === other) {
      s"$a == $other"
    } {
      s"$a != $other"
    }

  def lessOrEquals[A](other: A)(implicit ord: Order[A], F: Applicative[F]): PropositionT[F, A] =
    a => cond(a <= other) {
      s"$a <= $other"
    } {
      s"$a > $other"
    }

  def greaterOrEquals[A](other: A)(implicit ord: Order[A], F: Applicative[F]): PropositionT[F, A] =
    a => cond(a >= other) {
      s"$a >= $other"
    } {
      s"$a < $other"
    }

  def isDefined[A](implicit F: Applicative[F]): PropositionT[F, Option[A]] =
    a => cond(a.isDefined) {
      s"$a is defined"
    } {
      s"$a is undefined"
    }

  def isDefinedAndHolds[A](p: PropositionT[F, A])(implicit F: Applicative[F]): PropositionT[F, Option[A]] =
    proposition[Option[A]] {
      case Some(a) => p.check(a)
      case None => F.pure(falsum(s"unexpected None"))
    }

  def isEmpty[A](implicit F: Applicative[F]): PropositionT[F, Option[A]] =
    a => cond(a.isEmpty) {
      s"$a is empty"
    } {
      s"$a is defined"
    }

  def areOrdered[A](implicit ord: Order[A], F: Applicative[F]): PropositionT[F, List[A]] = {
    xs: List[A] =>
      def ordered(xs0: List[A]): TruthFix =
        xs0 match {
          case (x0 :: x1 :: ys) =>
            if (x0 <= x1) ordered(x1 :: ys)
            else Truth.falsum(s"$xs is not ordered")
          case (_ :: Nil) =>
            veritas(s"$xs is ordered")
          case Nil =>
            veritas(s"$xs is ordered")
        }
      F.pure(ordered(xs))
  }

}
