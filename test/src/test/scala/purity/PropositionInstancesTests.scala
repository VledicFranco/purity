package purity

import cats.laws.discipline.ContravariantTests
import purity.discipline.arbitrary._
import purity.discipline.eq._

class PropositionInstancesTests extends PuritySuite {

  checkAll("Proposition[String, ?]",
    ContravariantTests[Proposition[String, ?]].contravariant[Int, Int, Int])
}

