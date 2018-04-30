import cats.Id

package object purity {

  type Proposition[A] = PropositionT[Id, String, A]

  type PropositionK[K[_]] = PropositionTK[Id, String, K]

  object Proposition {

    def apply[A](f: A => Truth[String]): Proposition[A] = new PropositionT[Id, String, A](f)
  }
}
