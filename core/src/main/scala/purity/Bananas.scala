package purity

object Bananas {

  case class Fix[F[_]](unfix: F[Fix[F]])

  type Algebra[F[_], A] = F[A] => A
}
