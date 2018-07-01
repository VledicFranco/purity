package purity

import verification.TruthTracker
import TruthTracker._

package object implicits {

  implicit def toTruthOpsForTruth(a: Truth): OpsForTruth =
    new OpsForTruth(a)

  implicit def toTruthOpsForAny[A](x: A): OpsForAny[A] =
    new OpsForAny[A](x)

  implicit def toTruthOpsForString(name: String): OpsForString =
    new OpsForString(name)
}
