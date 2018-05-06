package purity

import cats._
import cats.implicits._
import matryoshka.Corecursive
import matryoshka.data.Mu
import purity.Truth.{False, True}

final case class PropositionTR[F[_], T, A](check: A => F[T])(implicit ev0: Corecursive.Aux[T, Truth]) {

  def contramap[B](f: B => A): PropositionTR[F, T, B] =
    PropositionTR(f andThen check)

  def in[B](f: B => A): PropositionTR[F, T, B] =
    contramap(f)

  def not(implicit F: Functor[F]): PropositionTR[F, T, A] =
    PropositionTR(a => F.map(check(a))(p => Truth.not(p)))

  def &&(q: PropositionTR[F, T, A])(implicit F: Applicative[F]): PropositionTR[F, T, A] =
    op(q)((x, y) => Truth.&&(x, y))

  def ||(q: PropositionTR[F, T, A])(implicit F: Applicative[F]): PropositionTR[F, T, A] =
    op(q)((x, y) => Truth.||(x, y))

  def ==>(q: PropositionTR[F, T, A])(implicit F: Applicative[F]): PropositionTR[F, T, A] =
    op(q)((x, y) => Truth.==>(x, y))

  def xor(q: PropositionTR[F, T, A])(implicit F: Applicative[F]): PropositionTR[F, T, A] =
    op(q)((x, y) => Truth.xor(x, y))

  def forAll(implicit F: Applicative[F]): PropositionTR[F, T, List[A]] =
    PropositionTR { xs: List[A] =>
      val truthsF = xs.traverse[F, T](check)
      F.map(truthsF) {
        _.foldLeft(Truth.isTrue("P holds"))(Truth.&&)
      }
    }

  private def op(q: PropositionTR[F, T, A])(f: (T, T) => T)(implicit F: Applicative[F]): PropositionTR[F, T, A] =
    PropositionTR(a => F.ap2[T, T, T](F.pure(f))(check(a), q.check(a)))

  override def toString: String = "PropositionT"
}

object PropositionTR extends PropositionTFunctions with PropositionTInstances

private[purity] trait PropositionTFunctions {

  def tautology[F[+_], T, A](tag: String)(implicit F: Applicative[F], ev0: Corecursive.Aux[T, Truth]): PropositionTR[F, T, A] =
    PropositionTR[F, T, A](_ => F.pure(ev0.embed(True[T](Some(tag)))))

  def contradiction[F[+_], T, A](tag: String)(implicit F: Applicative[F], ev0: Corecursive.Aux[T, Truth]): PropositionTR[F, T, A] =
    PropositionTR[F, T, A](_ => F.pure(ev0.embed(False[T](Some(tag)))))

  class OpsForFunction1[F[_], T, A](check: A => F[T])(implicit ev0: Corecursive.Aux[T, Truth]) {

    def prop: PropositionTR[F, T, A] = PropositionTR(check)
  }
}

private[purity] trait PropositionTInstances {

  implicit def stdContravariant[F[+_], P]: Contravariant[PropositionTR[F, P, ?]] =
    new Contravariant[PropositionTR[F, P, ?]] {
      override def contramap[A, B](fa: PropositionTR[F, P, A])(f: (B) => A): PropositionTR[F, P, B] =
        fa.contramap(f)
    }
}