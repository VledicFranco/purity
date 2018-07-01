package purity

import matryoshka.data.Mu

package object verification {

  type Truth = Mu[TruthF]

  type Proposition[A] = PropositionF[A, Truth]
}
