package purity.verification

import purity.PuritySuite
import Proposition._
import purity.implicits._

class SpecTests extends PuritySuite {

  val spec1 =
    "MySortFunction".domain1[List[Int]].image[List[Int]] { input =>
      areOrdered[Int] /\ sameLength(input) /\ positiveInt.forAllInList
    }

  test("1") {
    val res = spec1.verify(List(2, 5, 3))(xs => xs.sorted)
    println(res.report)
    assert(1 === 1)
  }
}
