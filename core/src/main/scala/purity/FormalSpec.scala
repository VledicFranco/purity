package purity

import cats.Functor
import matryoshka.data.Mu
import matryoshka.implicits._

case class FormalSpec1[F[_], A1, B](postCondition: A1 => PropositionT[F, Mu[Truth], B]) {

  def check(a1: A1)(program: A1 => B)(implicit F: Functor[F]): F[(String, Boolean)] = {
    val b = program(a1)
    F.map(postCondition(a1).check(b)) { result: Mu[Truth] =>
      result.cata(Truth.tracker)
    }
  }
}
