package purity

import cats.laws.discipline.ContravariantTests
import purity.discipline.arbitrary._
import purity.discipline.eq._

class PredicateInstancesTests extends PuritySuite {

  checkAll("Predicate[?]",
    ContravariantTests[Predicate].contravariant[Int, Int, Int])
}

