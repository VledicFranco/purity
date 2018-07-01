package purity.verification

import matryoshka.implicits._
import Truth._

object Spec {

  def report(result: (LatestDefinition, Definitions, Evaluation)): Result = {
    val (last, definitionsP, evaluation) = result
    val definitions = if (last == "") definitionsP else ("Anonymous", last) :: definitionsP
    val report = definitions.map {
      case (name, definition) => s"$name =/\\= $definition"
    }.mkString("\n\n")
    Result(report, evaluation)
  }

  case class Result(report: String, evaluation: Boolean)
}

case class Spec1[A1, B](postCondition: A1 => Proposition[B]) {

  def verify(a1: A1)(program: A1 => B): Spec.Result =
    Spec.report(postCondition(a1).check(program(a1)).cata(algebra))
}

case class Spec2T[F[_], A1, A2, B](postCondition: (A1, A2) => Proposition[B]) {

  def verify(a1: A1, a2: A2)(program: (A1, A2) => B): Spec.Result =
    Spec.report(postCondition(a1, a2).check(program(a1, a2)).cata(algebra))
}
