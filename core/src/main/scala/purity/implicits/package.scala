package purity

import matryoshka.Corecursive
import verification.Truth

package object implicits {

  implicit def toTruthOpsForTruth[T](a: T)(implicit ev0: Corecursive.Aux[T, Truth]): Truth.OpsForTruth[T] =
    new Truth.OpsForTruth[T](a)

  implicit def toTruthOpsForAny[X](x: X): Truth.OpsForAny[X] =
    new Truth.OpsForAny[X](x)
}
