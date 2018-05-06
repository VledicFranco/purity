package purity

import cats.Functor
import matryoshka.implicits._

case class Spec1T[F[_], A1, B](postCondition: A1 => PropositionT[F, B]) {

  def check(a1: A1)(program: A1 => B)(implicit F: Functor[F]): F[(String, Boolean)] =
    F.map(postCondition(a1).check(program(a1))) { result =>
      result.cata(Truth.tracker)
    }
}

object Spec1T {

  class OpsForFunction1[F[_], A1, B](postCondition: A1 => PropositionT[F, B]) {

    def spec: Spec1T[F, A1, B] = Spec1T[F, A1, B](postCondition)
  }
}

case class Spec2T[F[_], A1, A2, B](postCondition: (A1, A2) => PropositionT[F, B]) {

  def check(a1: A1, a2: A2)(program: (A1, A2) => B)(implicit F: Functor[F]): F[(String, Boolean)] =
    F.map(postCondition(a1, a2).check(program(a1, a2))) { result =>
      result.cata(Truth.tracker)
    }
}

object Spec2T {

  class OpsForFunction1[F[_], A1, A2, B](postCondition: (A1, A2) => PropositionT[F, B]) {

    def spec: Spec2T[F, A1, A2, B] = Spec2T[F, A1, A2, B](postCondition)
  }
}
