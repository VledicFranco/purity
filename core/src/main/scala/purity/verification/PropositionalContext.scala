package purity.verification

import cats.Applicative
import purity.verification.Truth.{falsum, veritas}

trait PropositionalContext[F[_]] {

  type Proposition[A] = PropositionT[F, A]

  def proposition[A](f: A => F[TruthFix]): Proposition[A] =
    a => f(a)

  def cond[A](p: Boolean)(tagT: => String)(tagF: => String)(implicit F: Applicative[F]): F[TruthFix] =
    F.pure {
      if (p) veritas(tagT)
      else falsum(tagF)
    }
}
