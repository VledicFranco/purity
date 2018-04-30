package purity

import cats._
import purity.Truth.{False, True}

trait PropositionTK[F[+_], P, K[_]] { p =>

  def check[A](ka: K[A]): F[Truth[P]]

  def contramapLower[A, B](f: B => K[A]): PropositionT[F, P, B] =
    PropositionT(f andThen check)

  def contramap[M[_]](f: M ~> K): PropositionTK[F, P, M] =
    new PropositionTK[F, P, M] {
      override def check[A](ma: M[A]): F[Truth[P]] =
        p.check(f.apply(ma))
    }

  def not(implicit F: Functor[F]): PropositionTK[F, P, K] =
    new PropositionTK[F, P, K] {
      override def check[A](ka: K[A]): F[Truth[P]] =
        F.map(p.check(ka))(t => t.not)
    }

  /*
  def &&[AA <: K](q: PropositionTK[F, P, AA])(implicit F: Applicative[F]): PropositionTK[F, P, AA] =
    op(q)(_ && _)

  def ||[AA <: K](q: PropositionTK[F, P, AA])(implicit F: Applicative[F]): PropositionTK[F, P, AA] =
    op(q)(_ || _)

  def ==>[AA <: K](q: PropositionTK[F, P, AA])(implicit F: Applicative[F]): PropositionTK[F, P, AA] =
    op(q)(_ ==> _)

  private def op[AA <: K](q: PropositionTK[F, P, AA])(f: (Truth[P], Truth[P]) => Truth[P])(implicit F: Applicative[F]): PropositionTK[F, P, AA] =
    PropositionTK(a => F.ap2[Truth[P], Truth[P], Truth[P]](F.pure(f))(check(a), q.check(a)))

  def andThen[AA <: K](q: PropositionTK[F, P, AA])(implicit F: Monad[F]): PropositionTK[F, P, AA] =
    PropositionTK[F, P, AA] { a =>
      F.flatMap(check(a)) {
        case True(_) => q.check(a)
        case f => F.pure(f)
      }
    }

  def optional(p: P)(implicit F: Applicative[F]): PropositionTK[F, P, Option[K]] =
    PropositionTK[F, P, Option[K]] {
      case Some(a) => check(a)
      case None => F.pure(True(p))
    }

  def required(p: P)(F: Applicative[F]): PropositionTK[F, P, Option[K]] =
    PropositionTK[F, P, Option[K]] {
      case Some(a) => check(a)
      case None => F.pure(False(p))
    }

  */
  override def toString: String = "PropositionTK"
}

object PropositionTK extends PropositionTKFunctions with PropositionTKInstances

private[purity] trait PropositionTKFunctions {

  /*
  def tautology[F[+_], P, A](p: P)(implicit F: Applicative[F]): PropositionTK[F, P, A] =
    PropositionTK[F, P, A](_ => F.pure(True(p)))

  def contradiction[F[+_], P, A](p: P)(implicit F: Applicative[F]): PropositionTK[F, P, A] =
    PropositionTK[F, P, A](_ => F.pure(False(p)))
    */
}

private[purity] trait PropositionTKInstances {

  /*
  implicit def stdContravariant[F[+_], P]: Contravariant[PropositionTK[F, P, ?]] =
    new Contravariant[PropositionTK[F, P, ?]] {
      override def contramap[A, B](fa: PropositionTK[F, P, A])(f: (B) => A): PropositionTK[F, P, B] =
        fa.contramap(f)
    }
    */
}