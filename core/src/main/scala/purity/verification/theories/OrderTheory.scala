package purity.verification.theories

import cats.implicits._
import cats.{Applicative, Order}
import purity.verification.{PropositionalContext, Truth, TruthFix}
import purity.verification.Truth.veritas

trait OrderTheory[F[_]] extends PropositionalContext[F] {

  def lessOrEquals[A](other: A)(implicit ord: Order[A], F: Applicative[F]): Proposition[A] =
    a => cond(a <= other) {
      s"$a <= $other"
    } {
      s"$a > $other"
    }

  def greaterOrEquals[A](other: A)(implicit ord: Order[A], F: Applicative[F]): Proposition[A] =
    a => cond(a >= other) {
      s"$a >= $other"
    } {
      s"$a < $other"
    }

  def areOrdered[A](implicit ord: Order[A], F: Applicative[F]): Proposition[List[A]] = {
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
