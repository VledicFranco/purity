package purity.script

import cats.Id
import purity.PuritySuite
import cats.laws.discipline._

class InstancesTests extends PuritySuite {

  case class Dependencies()

  checkAll("ScriptT[Either[Throwable, ?], Dependencies, Throwable, ?]",
    MonadTests[ScriptT[Either[Throwable, ?], Dependencies, Throwable, ?]].monad[Int, Int, Int])
}
