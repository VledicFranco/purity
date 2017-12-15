package purity.script

import purity.PuritySuite
import cats.laws.discipline._
import purity.discipline.arbitrary._
import purity.discipline.eq._

class InstancesTests extends PuritySuite {

  type IOMock[+A] = Either[Throwable, A]

  type Script[+A] = ScriptT[IOMock, Int, Int, A]

  case object Only extends Exception("Only")

  checkAll("ScriptT[Either[Throwable, ?], Dependencies, Int, ?]",
    MonadTests[ScriptT[IOMock, Int, Int, ?]].monad[Int, Int, Int])

  test("TEST") {
    import cats._
    val Ap: Applicative[Script] = Applicative[Script]
    val a: Script[Int] = ScriptT[IOMock, Int, Int, Int]((_: Int) => Left(Only))
    //val aa: Script[Int] = ScriptT[IOMock, Int, Int, Int]((_: Int) => Right(L))
    val b = Ap.ap(Ap.pure({x: Int => x}))(a)
    a.definition(1) shouldEqual b.definition(1)
  }
}
