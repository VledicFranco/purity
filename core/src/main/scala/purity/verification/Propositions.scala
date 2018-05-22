package purity.verification

import cats.implicits._
import cats.{Applicative, Order}
import matryoshka.Corecursive

class Propositions[F[_]] {

  def areOrdered[A](implicit ord: Order[A], F: Applicative[F]): PropositionT[F, List[A]] = {
    implicit val ev0 = implicitly[Corecursive.Aux[TruthValue, Truth]]
    PropositionT { xs: List[A] =>
      def ordered(xs0: List[A]): TruthValue =
        xs0 match {
          case (x0 :: x1 :: ys) =>
            if (x0 <= x1) ordered(x1 :: ys)
            else Truth.isFalse(s"$xs is not ordered")
          case (_ :: Nil) =>
            Truth.isTrue(s"$xs is ordered")
          case Nil =>
            Truth.isTrue(s"$xs is ordered")
        }

      F.pure(ordered(xs))
    }
  }

  def |<=|[A](other: A)(implicit ord: Order[A], F: Applicative[F]): PropositionT[F, A] = {
    implicit val ev0 = implicitly[Corecursive.Aux[TruthValue, Truth]]
    PropositionT { x =>
      F.pure(
        if(x <= other) Truth.isTrue(s"$x <= $other")
        else Truth.isFalse(s"$x > $other")
      )
    }
  }

  def |>=|[A](other: A)(implicit ord: Order[A], F: Applicative[F]): PropositionT[F, A] = {
    implicit val ev0 = implicitly[Corecursive.Aux[TruthValue, Truth]]
    PropositionT { x =>
      F.pure(
        if (x >= other) Truth.isTrue(s"$x >= $other")
        else Truth.isFalse(s"$x < $other")
      )
    }
  }
}
