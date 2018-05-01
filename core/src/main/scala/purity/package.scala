import cats.Id
import matryoshka.Corecursive
import matryoshka.data.Mu

package object purity {

  type Proposition[A] = PropositionT[Id, Mu[Truth], A]

  //type PropositionK[K[_]] = PropositionTK[Id, Mu[Truth], K]

  object Proposition {

    def apply[A](f: A => Mu[Truth]): Proposition[A] = new PropositionT[Id, Mu[Truth], A](f)
  }
}
