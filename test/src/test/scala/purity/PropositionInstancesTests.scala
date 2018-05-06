package purity

import cats.{Id, Order}
import cats.implicits._
import cats.laws.discipline.ContravariantTests
import purity.Truth._
import purity.discipline.arbitrary._
import purity.discipline.eq._

class PropositionInstancesTests extends PuritySuite {

  /*
  checkAll("Proposition[String, ?]",
    ContravariantTests[Proposition[String, ?]].contravariant[Int, Int, Int])

  case class GameState()

  def isOrdered[A](tag: String)(implicit order: Order[A]): Proposition[List[A]] = {
    Proposition { xs =>
      if(ordered(xs)) True(tag)
      else False(tag)
    }
  }

  def ordered[A](xs: List[A])(implicit order: Order[A]): Boolean =
    xs match {
      case (x0 :: x1 :: ys) if x0 <= x1 =>
        ordered(x1 :: ys)
      case (_ :: Nil) =>
        true
      case Nil =>
        true
      case _ =>
        false
    }

  test("test 1") {

  }
    */
}

