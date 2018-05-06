package purity

import cats.Eq
import cats.effect.laws.util.TestContext
import cats.effect.{Effect, IO}
import cats.instances.AllInstances
import cats.syntax.{AllSyntax, EqOps}
import org.scalactic.source
import org.scalatest._
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.typelevel.discipline.Laws
import org.typelevel.discipline.scalatest.Discipline
import purity.Truth.{False, True}
import purity.logging.{LogLine, LoggerContainer}
import purity.script.ScriptDsl

import scala.concurrent.{Future, Promise}

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

trait ScriptSuite[F[+_]] extends AsyncPuritySuite with ScriptDsl[F] {

  /*
  protected def assertThat[D, E, A]
      (script: Script[D, E, A])
      (f: A => Assertion)
      (implicit dependencies: D with LoggerContainer[F], effect: Effect[F]): Future[Assertion] = {
    val p = Promise[Assertion]
    val Fa = script
      .logError(LogLine.fatal)
      .fold(dependencies, e => fail(s"Test failed with Script failure: $e"), f)
    effect.runAsync(Fa) {
      case Left(e) => IO { p.failure(e) }
      case Right(a0) => IO { p.success(a0) }
    }.unsafeRunAsync(_ => ())
    p.future
  }

  protected def proveThat[D, E, A]
      (script: Script[D, E, A])
      (p: Proposition[String, A])
      (implicit dependencies: D with LoggerContainer[F], effect: Effect[F]): Future[Assertion] =
    assertThat(script)(propositionAssertion(p))

  protected def proveThat[E, A]
      (script: Independent[E, A])
      (p: Proposition[String, A])
      (implicit dependencies: LoggerContainer[F], effect: Effect[F]): Future[Assertion] =
    assertThat(script)(propositionAssertion(p))(dependencies, effect)

  protected case class AfterScript[D, E, A](script: Script[D, E, A])(implicit dependencies: D with LoggerContainer[F]) {

    def itHoldsThat[B](other: => B)(p: Proposition[String, B])(implicit effect: Effect[F]): Future[Assertion] =
      assertThat(script){ _:A => propositionAssertion(p)(other) }
  }

  protected def proveThatAfter[D, E, A]
      (script: Script[D, E, A])
      (implicit dependencies: D with LoggerContainer[F]): AfterScript[D, E, A] =
    AfterScript(script)(dependencies)

  protected def proveThatAfter[E, A]
      (script: Independent[E, A])
      (implicit logger: LoggerContainer[F]): AfterScript[Any, E, A] =
    AfterScript(script)(logger)

  protected def propositionAssertion[A](p: Proposition[String, A]): A => Assertion =
    a => p.check(a) match {
      case True           => assert(1 == 1)
      case False(reasons) => fail(reasons.toList.mkString(" and "))
    }
    */
}
