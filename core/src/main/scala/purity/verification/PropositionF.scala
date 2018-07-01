package purity.verification

import cats.{Contravariant, Order}
import cats.implicits._
import matryoshka.Corecursive

import scala.annotation.tailrec

case class PropositionF[A, T](check: A => T)(implicit cor: Corecursive.Aux[T, TruthF]) {

  val functions: TruthFFunctions[T] = {
    new TruthFFunctions[T] {}
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

object PropositionF extends PropositionTInstances

abstract class PropositionFFunctions[T](implicit cor: Corecursive.Aux[T, TruthF]) extends TruthFFunctions[T]()(cor) {

  def apply[A](f: A => T): PropositionF[A, T] =
    new PropositionF[A, T](f)

  def tautology[A]: PropositionF[A, T] =
    PropositionF[A, T](_ => veritas)

  def contradiction[A]: PropositionF[A, T] =
    PropositionF[A, T](_ => falsum)

  def define[A](name: String, f: A => T): PropositionF[A, T] =
    new PropositionF[A, T](a => definition(f(a), name))

  def |<=|[A](other: A)(implicit ord: Order[A]): PropositionF[A, T] =
    define("LessThan", (a: A) => cond(a <= other))

  def |>=|[A](other: A)(implicit ord: Order[A]): PropositionF[A, T] =
    define("GreaterThan", (a: A) => cond(a <= other))

  def areOrdered[A](implicit ord: Order[A]): PropositionF[List[A], T] =
    define("Ordered", { xs: List[A] =>
      @tailrec def ordered(xs0: List[A]): T =
        xs0 match {
          case x0 :: x1 :: ys =>
            if (x0 <= x1) ordered(x1 :: ys)
            else falsum
          case _ :: Nil =>
            veritas
          case Nil =>
            veritas
        }
      ordered(xs)
    })

  class OpsForString(name: String) {

    def =/\=[A](f: A => T): PropositionF[A, T] =
      define(name, f)
  }
}

private[purity] trait PropositionTInstances {

  implicit def stdContravariant[T](implicit cor: Corecursive.Aux[T, TruthF]): Contravariant[PropositionF[?, T]] =
    new Contravariant[PropositionF[?, T]] {
      override def contramap[A, B](fa: PropositionF[A, T])(f: B => A): PropositionF[B, T] =
        fa.contramap(f)
    }
}