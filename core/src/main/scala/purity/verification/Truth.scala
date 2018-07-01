package purity.verification

import matryoshka.Algebra
import TruthF._

object Truth extends TruthFFunctions[Truth] {

  private[purity] type DefinitionName = String

  private[purity] type LatestDefinition = String

  private[purity] type Definitions = List[(DefinitionName, LatestDefinition)]

  private[purity] type Evaluation = Boolean

  val tracker: Algebra[TruthF, (LatestDefinition, Definitions, Evaluation)] = {
    case True() =>
      ("true", Nil, true)

    case False() =>
      ("false", Nil, false)

    case Not((latest, definitions, p)) =>
      (s"!$latest", definitions, !p)

    case And((pLatest, pDefinitions, p), (qLatest, qDefinitions, q)) =>
      (s"($pLatest /\\ $qLatest)", pDefinitions ++ qDefinitions, p && q)

    case Or((pLatest, pDefinitions, p), (qLatest, qDefinitions, q)) =>
      (s"($pLatest \\/ $qLatest)", pDefinitions ++ qDefinitions, p || q)

    case IfThenElse((pLatest, pDefinitions, p), (qLatest, qDefinitions, q), (rLatest, rDefinitions, r)) =>
      (s"if ($pLatest) then ($qLatest) else ($rLatest)", pDefinitions ++ qDefinitions ++ rDefinitions, if (p) q else r)

    case Definition(name, (latest, definitions, p)) =>
      val evalSymbol =
        if(p) Console.GREEN + "☑ " + Console.RESET
        else Console.RED + "☒ " + Console.RESET
      val name0 = evalSymbol + Console.MAGENTA + name + Console.RESET
      (s"$name0", (s"$name0", latest) :: definitions, p)
  }
}
