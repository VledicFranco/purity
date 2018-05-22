package purity

import cats.Id
import matryoshka.data.Mu

package object verification {

  type TruthValue = Mu[Truth]

  type Proposition[A] = PropositionTR[Id, Mu[Truth], A]

  type PropositionT[F[_], A] = PropositionTR[F, Mu[Truth], A]

  type Spec1[A1, B] = Spec1T[Id, A1, B]

  type Spec2[A1, A2, B] = Spec2T[Id, A1, A2, B]

  object Proposition {

    def apply[A](f: A => Mu[Truth]): Proposition[A] =
      new PropositionTR[Id, Mu[Truth], A](f)
  }

  object PropositionT {

    def apply[F[_], A](f: A => F[Mu[Truth]]): PropositionT[F, A] =
      new PropositionTR[F, Mu[Truth], A](f)
  }

  object Spec1 {

    def apply[A1, B](f: A1 => Proposition[B]): Spec1[A1, B] =
      new Spec1T[Id, A1, B](f)
  }

  object Spec2 {

    def apply[A1, A2, B](f: (A1, A2) => Proposition[B]): Spec2[A1, A2, B] =
      new Spec2T[Id, A1, A2, B](f)
  }
}
