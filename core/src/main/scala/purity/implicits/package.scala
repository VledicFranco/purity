package purity

import verification.{Proposition, Truth}

package object implicits {

  implicit def toTruthOpsForTruth(a: Truth): Truth.OpsForTruth =
    new Truth.OpsForTruth(a)

  implicit def toTruthOpsForAny[A](x: A): Truth.OpsForAny[A] =
    new Truth.OpsForAny[A](x)

  implicit def toPropositionOpsForString(name: String): Proposition.OpsForString =
    new Proposition.OpsForString(name)
}
