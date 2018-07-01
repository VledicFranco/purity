package purity.verification

import matryoshka.implicits._
import Truth._

object Spec {

  def report(name: String, result: (LatestDefinition, Definitions, Evaluation)): Result = {
    val (last, definitionsP, evaluation) = result
    val evalSymbol =
      if(evaluation) Console.GREEN + "☑ " + Console.RESET
      else Console.RED + "☒ " + Console.RESET
    val name0 = evalSymbol + Console.MAGENTA + name + Console.RESET
    val definitions =
      if (last == "") definitionsP
      else (name0, last) :: definitionsP
    val report = definitions.map {
      case (name, definition) =>
        s"$name =/\\= $definition"
    }.mkString("\n")
    val evReport =
      if (evaluation) Console.GREEN + "Verification passed:\n" + Console.RESET
      else Console.RED + "Verification failed:\n" + Console.RESET
    Result(evReport + report, evaluation)
  }

  case class Result(report: String, evaluation: Boolean)

  class SpecOpsForString(name: String) {

    def domain1[A1]: Input1Spec[A1] = new Input1Spec[A1](name)

    def domain2[A1, A2]: Input2Spec[A1, A2] = new Input2Spec[A1, A2](name)
  }

  class Input1Spec[A1](name: String) {

    def image[B](f: A1 => Proposition[B]) = Spec1(name, f)
  }

  class Input2Spec[A1, A2](name: String) {

    def image[B](f: (A1, A2) => Proposition[B]) = Spec2(name, f)
  }
}

case class Spec1[A1, B](name: String, postCondition: A1 => Proposition[B]) {

  def verify(a1: A1)(program: A1 => B): Spec.Result =
    Spec.report(name, postCondition(a1).check(program(a1)).cata(tracker))
}

case class Spec2[A1, A2, B](name: String, postCondition: (A1, A2) => Proposition[B]) {

  def verify(a1: A1, a2: A2)(program: (A1, A2) => B): Spec.Result =
    Spec.report(name, postCondition(a1, a2).check(program(a1, a2)).cata(tracker))
}
