package purity.verification

import cats.Eq
import matryoshka._
import scalaz.Functor
import purity.verification.TruthF._

sealed trait TruthF[T]

object TruthF extends TruthInstances {

  case class True[T]() extends TruthF[T]

  case class False[T]() extends TruthF[T]

  case class Not[T](p: T) extends TruthF[T]

  case class And[T](p: T, q: T) extends TruthF[T]

  case class Or[T](p: T, q: T) extends TruthF[T]

  case class IfThenElse[T](p: T, q: T, r: T) extends TruthF[T]

  case class Definition[T](name: String, p: T) extends TruthF[T]

  case class Contradiction(track: String) extends RuntimeException
}

abstract class TruthFFunctions[T](implicit cor: Corecursive.Aux[T, TruthF]) { functions =>

  def definition(p: T, str: String): T =
    cor.embed(Definition(str, p))

  def veritas: T =
    cor.embed(True[T]())

  def falsum: T =
    cor.embed(False[T]())

  def not(p: T): T =
    cor.embed(Not[T](p))

  def &&(p: T, q: T): T =
    cor.embed(And[T](p, q))

  def ||(p: T, q: T): T =
    cor.embed(Or[T](p, q))

  def ==>(p: T, q: T): T =
    ||(not(p), q)

  def xor(p: T, q: T): T =
    &&(||(p, q), not(&&(p, q)))

  def cond[A](p: Boolean): T =
    if(p) veritas else falsum

  def =:=[A](x: A, y: A)(implicit eq: Eq[A]): T =
    cond(eq.eqv(x, y))

  class OpsForTruth(p: T) {

    def not: T = functions.not(p)

    def &&(q: T): T = functions.&&(p, q)

    def ||(q: T): T = functions.||(p, q)

    def ==>(q: T): T = functions.==>(p, q)

    def xor(q: T): T = functions.xor(p, q)
  }

  class OpsForAny[A](x: => A) {

    def =:=(y: => A)(implicit ev1: Eq[A]): T =
      functions.=:=[A](x, y)
  }
}

private[purity] trait TruthInstances {

  implicit val stdFunctorOfTruth: Functor[TruthF] =
    new Functor[TruthF] {
      override def map[A, B](fa: TruthF[A])(f: A => B): TruthF[B] =
        fa match {
          case True() => True()
          case False() => False()
          case Not(p) => Not(f(p))
          case And(p, q) => And(f(p), f(q))
          case Or(p, q) => Or(f(p), f(q))
          case IfThenElse(p, q, r) => IfThenElse(f(p), f(q), f(r))
          case Definition(name, p) => Definition(name, f(p))
        }
    }
}
