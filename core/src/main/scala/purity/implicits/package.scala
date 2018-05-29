package purity

import verification.{Truth, TruthFix}

package object implicits {

  implicit def toTruthOpsForTruth(a: TruthFix): Truth.OpsForTruth =
    new Truth.OpsForTruth(a)

  implicit def toTruthOpsForAny[X](x: X): Truth.OpsForAny[X] =
    new Truth.OpsForAny[X](x)
}
