package purity.verification

import cats.Contravariant
import matryoshka.Corecursive
import purity.verification.TruthF.{False, True}

case class PropositionF[A, T](check: A => T)(implicit cor: Corecursive.Aux[T, TruthF]) {

  val functions: TruthFunctions[T] = {
    new TruthFunctions[T] {}
  }

  def contramap[B](f: B => A): PropositionF[B, T] =
    PropositionF(f andThen check)

  def in[B](f: B => A): PropositionF[B, T] =
    contramap(f)

  def not: PropositionF[A, T] =
    PropositionF(a => functions.not(check(a)))

  def &&(q: PropositionF[A, T]): PropositionF[A, T] =
    op(q)(functions.&&)

  def ||(q: PropositionF[A, T]): PropositionF[A, T] =
    op(q)(functions.||)

  def ==>(q: PropositionF[A, T]): PropositionF[A, T] =
    op(q)(functions.==>)

  def xor(q: PropositionF[A, T]): PropositionF[A, T] =
    op(q)(functions.xor)

  def onList: PropositionF[List[A], T] =
    PropositionF { xs: List[A] =>
      xs.foldLeft(functions.veritas)((x, y) => functions.&&(x, check(y)))
    }

  private def op(q: PropositionF[A, T])(f: (T, T) => T): PropositionF[A, T] =
    PropositionF(a => f(check(a), q.check(a)))

  override def toString: String = "PropositionF"
}

object PropositionF extends PropositionFFunctions with PropositionTInstances

private[purity] trait PropositionFFunctions {

  def tautology[A, T](implicit cor: Corecursive.Aux[T, TruthF]): PropositionF[A, T] =
    PropositionF[A, T](_ => cor.embed(True[T]()))

  def contradiction[A, T](implicit cor: Corecursive.Aux[T, TruthF]): PropositionF[A, T] =
    PropositionF[A, T](_ => cor.embed(False[T]()))
}

private[purity] trait PropositionTInstances {

  implicit def stdContravariant[T](implicit cor: Corecursive.Aux[T, TruthF]): Contravariant[PropositionF[?, T]] =
    new Contravariant[PropositionF[?, T]] {
      override def contramap[A, B](fa: PropositionF[A, T])(f: B => A): PropositionF[B, T] =
        fa.contramap(f)
    }
}