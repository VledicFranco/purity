package purity.verification

import cats._
import cats.implicits._

trait PropositionT[F[_], A] {

  def check(a: A): F[TruthFix]

  def contramap[B](f: B => A): PropositionT[F, B] =
    PropositionT(f andThen check)

  def in[B](f: B => A): PropositionT[F, B] =
    contramap(f)

  def not(implicit F: Functor[F]): PropositionT[F, A] =
    PropositionT(a => F.map(check(a))(p => Truth.not(p)))

  def &&(q: PropositionT[F, A])(implicit F: Applicative[F]): PropositionT[F, A] =
    op(q)((x, y) => Truth.&&(x, y))

  def ||(q: PropositionT[F, A])(implicit F: Applicative[F]): PropositionT[F, A] =
    op(q)((x, y) => Truth.||(x, y))

  def ==>(q: PropositionT[F, A])(implicit F: Applicative[F]): PropositionT[F, A] =
    op(q)((x, y) => Truth.==>(x, y))

  def xor(q: PropositionT[F, A])(implicit F: Applicative[F]): PropositionT[F, A] =
    op(q)((x, y) => Truth.xor(x, y))

  def forAll(implicit F: Applicative[F]): PropositionT[F, List[A]] =
    PropositionT { xs: List[A] =>
      val truthsF = xs.traverse[F, TruthFix](check)
      F.map(truthsF) {
        _.foldLeft(Truth.veritas("P holds"))(Truth.&&)
      }
    }

  private def op(q: PropositionT[F, A])(f: (TruthFix, TruthFix) => TruthFix)(implicit F: Applicative[F]): PropositionT[F, A] =
    PropositionT(a => F.ap2(f.pure[F])(check(a), q.check(a)))

  override def toString: String = "PropositionT"
}

object PropositionT extends PropositionTFunctions with PropositionTInstances {

  def apply[F[_], A](check0: A => F[TruthFix]): PropositionT[F, A] =
    a => check0(a)
}

private[purity] trait PropositionTFunctions {

  def tautology[F[_], A](tag: String)(implicit F: Applicative[F]): PropositionT[F, A] =
    PropositionT[F, A](_ => F.pure(Truth.veritas(tag)))

  def contradiction[F[_], A](tag: String)(implicit F: Applicative[F]): PropositionT[F, A] =
    PropositionT[F, A](_ => F.pure(Truth.falsum(tag)))
}

private[purity] trait PropositionTInstances {

  implicit def stdContravariant[F[_]]: Contravariant[PropositionT[F, ?]] =
    new Contravariant[PropositionT[F, ?]] {
      override def contramap[A, B](fa: PropositionT[F, A])(f: (B) => A): PropositionT[F, B] =
        fa.contramap(f)
    }
}