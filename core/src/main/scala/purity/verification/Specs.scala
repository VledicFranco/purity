package purity.verification

trait Specs[F[_]] {

  type Spec1[A1, B] = Spec1T[F, A1, B]

  type Spec2[A1, A2, B] = Spec2T[F, A1, A2, B]

  object Spec1 {

    def apply[A1, B](f: A1 => PropositionT[F, B]): Spec1[A1, B] =
      (a1: A1) => f(a1)
  }

  object Spec2 {

    def apply[A1, A2, B](f: (A1, A2) => PropositionT[F, B]): Spec2[A1, A2, B] =
      (a1: A1, a2: A2) => f(a1, a2)
  }
}
