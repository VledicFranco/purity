package purity

import cats.Id
import matryoshka.Corecursive

package object implicits {

  implicit def toTruthOpsForTruth[T](a: T)(implicit ev0: Corecursive.Aux[T, Truth]): Truth.OpsForTruth[T] =
    new Truth.OpsForTruth[T](a)

  implicit def toTruthOpsForAny[X](x: X): Truth.OpsForAny[X] =
    new Truth.OpsForAny[X](x)

  implicit def toPropositionOpsForFunction1[F[_], T, A](check: A => F[T])(implicit ev0: Corecursive.Aux[T, Truth]): PropositionTR.OpsForFunction1[F, T, A] =
    new PropositionTR.OpsForFunction1[F, T, A](check)

  implicit def toPropositionOpsForFunction1Id[T, A](check: A => T)(implicit ev0: Corecursive.Aux[T, Truth]): PropositionTR.OpsForFunction1[Id, T, A] =
    new PropositionTR.OpsForFunction1[Id, T, A](check)

  implicit def toSpec1OpsForFunction1[F[_], A1, B](postCondition: A1 => PropositionT[F, B]): Spec1T.OpsForFunction1[F, A1, B] =
    new Spec1T.OpsForFunction1[F, A1, B](postCondition)

  implicit def toSpec1OpsForFunction1Id[A1, B](postCondition: A1 => Proposition[B]): Spec1T.OpsForFunction1[Id, A1, B] =
    new Spec1T.OpsForFunction1[Id, A1, B](postCondition)
}
