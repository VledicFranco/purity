package purity

import verification.{Proposition, Spec, Truth}

package object implicits {

  implicit def toTruthOpsForTruth(a: Truth): Truth.OpsForTruth =
    new Truth.OpsForTruth(a)

  implicit def toTruthOpsForAny[A](x: A): Truth.OpsForAny[A] =
    new Truth.OpsForAny[A](x)

  implicit def toPropositionOpsForString(name: String): Proposition.OpsForString =
    new Proposition.OpsForString(name)

  implicit def toSpec1OpsForString(name: String): Spec.SpecOpsForString =
    new Spec.SpecOpsForString(name)
}
