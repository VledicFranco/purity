package purity.script

import purity.PuritySuite
import cats.laws.discipline._
import purity.discipline.arbitrary._
import purity.discipline.eq._

class InstancesTests extends PuritySuite {

  type IOMock[+A] = Either[Throwable, A]

  checkAll("ScriptT[Either[Throwable, ?], Dependencies, Int, ?]",
    MonadErrorTests[ScriptT[IOMock, Int, Int, ?], Int].monadError[Int, Int, Int])
}
