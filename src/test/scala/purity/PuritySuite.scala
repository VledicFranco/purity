package purity

import cats.Eq
import cats.instances.AllInstances
import cats.syntax.{ AllSyntax, EqOps }
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalatest.{ FunSuite, FunSuiteLike, Matchers }
import org.typelevel.discipline.scalatest.Discipline

trait PuritySuite extends FunSuite
    with Matchers
    with GeneratorDrivenPropertyChecks
    with Discipline
    with AllInstances
    with AllSyntax { self: FunSuiteLike ⇒

  // disable Eq syntax (by making `catsSyntaxEq` not implicit), since it collides
  // with scalactic's equality
  override def catsSyntaxEq[A: Eq](a: A): EqOps[A] = new EqOps[A](a)

  def even(i: Int): Boolean = i % 2 == 0

  val evenPf: PartialFunction[Int, Int] = { case i if even(i) ⇒ i }
}
