package purity

import cats.Functor

case class FormalSpec1[F[+_], A1, B](postCondition: A1 => PropositionT[F, String, B]) {

  def check(program: A1 => B)(a: A1)(implicit F: Functor[F]): F[(String, Boolean)] = {
    val b = program(a)
    F.map(postCondition(a).check(b)) { truth =>
      truth.fold[(String, Boolean)](Truth.tracker)
    }
  }
}
