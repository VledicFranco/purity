package purity

import cats._
import purity.Truth.{False, True}

final case class PropositionT[F[_], P, A](check: A => F[Truth[P]]) extends AnyVal { p =>

  def contramap[B](f: B => A): PropositionT[F, P, B] =
    PropositionT(f andThen check)

  def not(implicit F: Functor[F]): PropositionT[F, P, A] =
    PropositionT(a => F.map(check(a))(t => t.not))

  def &&(q: PropositionT[F, P, A])(implicit F: Applicative[F]): PropositionT[F, P, A] =
    op(q)(_ && _)

  def ||(q: PropositionT[F, P, A])(implicit F: Applicative[F]): PropositionT[F, P, A] =
    op(q)(_ || _)

  def ==>(q: PropositionT[F, P, A])(implicit F: Applicative[F]): PropositionT[F, P, A] =
    op(q)(_ ==> _)

  private def op(q: PropositionT[F, P, A])(f: (Truth[P], Truth[P]) => Truth[P])(implicit F: Applicative[F]): PropositionT[F, P, A] =
    PropositionT(a => F.ap2[Truth[P], Truth[P], Truth[P]](F.pure(f))(check(a), q.check(a)))

  def andThen(q: PropositionT[F, P, A])(implicit F: Monad[F]): PropositionT[F, P, A] =
    PropositionT[F, P, A] { a =>
      F.flatMap(check(a)) {
        case True(_) => q.check(a)
        case f => F.pure(f)
      }
    }

  def optional(p: P)(implicit F: Applicative[F]): PropositionT[F, P, Option[A]] =
    PropositionT[F, P, Option[A]] {
      case Some(a) => check(a)
      case None => F.pure(True(p))
    }

  def required(p: P)(F: Applicative[F]): PropositionT[F, P, Option[A]] =
    PropositionT[F, P, Option[A]] {
      case Some(a) => check(a)
      case None => F.pure(False(p))
    }

  override def toString: String = "PropositionT"
}

object PropositionT extends PropositionTFunctions with PropositionTInstances

private[purity] trait PropositionTFunctions {

  def tautology[F[+_], P, A](p: P)(implicit F: Applicative[F]): PropositionT[F, P, A] =
    PropositionT[F, P, A](_ => F.pure(True(p)))

  def contradiction[F[+_], P, A](p: P)(implicit F: Applicative[F]): PropositionT[F, P, A] =
    PropositionT[F, P, A](_ => F.pure(False(p)))
}

private[purity] trait PropositionTInstances {

  implicit def stdContravariant[F[+_], P]: Contravariant[PropositionT[F, P, ?]] =
    new Contravariant[PropositionT[F, P, ?]] {
      override def contramap[A, B](fa: PropositionT[F, P, A])(f: (B) => A): PropositionT[F, P, B] =
        fa.contramap(f)
    }
}