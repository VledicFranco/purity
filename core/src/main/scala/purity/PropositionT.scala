package purity

import cats._
import matryoshka.Corecursive
import matryoshka.data.Mu
import matryoshka.implicits._
import purity.Truth.True

final case class PropositionT[F[_], T, A](check: A => F[T])(implicit ev0: Corecursive.Aux[T, Truth]) {

  def contramap[B](f: B => A): PropositionT[F, T, B] =
    PropositionT(f andThen check)

  def not(implicit F: Functor[F]): PropositionT[F, T, A] =
    PropositionT(a => F.map(check(a))(p => Truth.not(p)))

  def &&(q: PropositionT[F, T, A])(implicit F: Applicative[F]): PropositionT[F, T, A] =
    op(q)((x, y) => Truth.&&(x, y))

  /*
  def ||(q: PropositionT[F, T, A])(implicit F: Applicative[F]): PropositionT[F, T, A] =
    op(q)(_ || _)

  def ==>(q: PropositionT[F, T, A])(implicit F: Applicative[F]): PropositionT[F, T, A] =
    op(q)(_ ==> _)
    */

  private def op(q: PropositionT[F, T, A])(f: (T, T) => T)(implicit F: Applicative[F]): PropositionT[F, T, A] =
    PropositionT(a => F.ap2[T, T, T](F.pure(f))(check(a), q.check(a)))

  override def toString: String = "PropositionT"
}

object PropositionT extends PropositionTFunctions with PropositionTInstances

private[purity] trait PropositionTFunctions {

  /*
  def tautology[F[+_], A](tag: String)(implicit F: Applicative[F]): PropositionT[F, Mu[Truth], A] =
    PropositionT[F, Mu[Truth], A](_ => F.pure(True[Mu[Truth]](Some(tag)).embed))

  def contradiction[F[+_], T, A](p: T)(implicit F: Applicative[F], ev0: Corecursive.Aux[T, Truth]): PropositionT[F, T, A] =
    PropositionT[F, T, A](_ => F.pure(False(p)))
    */
}

private[purity] trait PropositionTInstances {

  implicit def stdContravariant[F[+_], P]: Contravariant[PropositionT[F, P, ?]] =
    new Contravariant[PropositionT[F, P, ?]] {
      override def contramap[A, B](fa: PropositionT[F, P, A])(f: (B) => A): PropositionT[F, P, B] =
        fa.contramap(f)
    }
}