package purity.script

import cats.effect.IO
import cats.effect.laws.discipline.EffectTests
import cats.effect.laws.discipline.arbitrary._
import cats.effect.laws.util.TestContext
import cats.laws.discipline._
import purity.PuritySuite
import purity.discipline.arbitrary._
import purity.discipline.eq._

class InstancesTests extends PuritySuite {

  implicit val ds: Int = 1
  implicit val ec: TestContext = TestContext()

  checkAll("ScriptT[IO, Dependencies, Int, ?]",
    MonadErrorTests[ScriptT[IO, Int, Int, ?], Int].monadError[Int, Int, Int])

  checkAllAsync("ScriptT[IO, Dependencies, Int, ?]",
    implicit ec ⇒ EffectTests[ScriptT[IO, Int, Throwable, ?]].effect[Int, Int, Int])

}
