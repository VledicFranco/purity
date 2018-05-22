package purity

import cats.Eq
import cats.effect.laws.util.TestContext
import cats.instances.AllInstances
import cats.syntax.{AllSyntax, EqOps}
import org.scalactic.source
import org.scalatest._
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.typelevel.discipline.Laws
import org.typelevel.discipline.scalatest.Discipline

trait AsyncPuritySuite extends AsyncFunSpec with CommonPuritySuite

trait PuritySuite extends FunSuite with Discipline with CommonPuritySuite {

  def testAsync[A](name: String, tags: Tag*)(f: TestContext => Unit)(implicit pos: source.Position): Unit =
    test(name, tags:_*)(f(TestContext()))(pos)

  /** For discipline tests. */
  def checkAllAsync(name: String, f: TestContext => Laws#RuleSet) {
    val context = TestContext()
    val ruleSet = f(context)

    for ((id, prop) ← ruleSet.all.properties)
      test(name + "." + id) {
        check(prop)
      }
  }
}

trait CommonPuritySuite extends Matchers
  with GeneratorDrivenPropertyChecks
  with AllInstances
  with AllSyntax {
  // disable Eq syntax (by making `catsSyntaxEq` not implicit), since it collides
  // with scalactic's equality
  override def catsSyntaxEq[A: Eq](a: A): EqOps[A] = new EqOps[A](a)
}
