package purity.verification

import matryoshka.Algebra
import matryoshka.data.Mu
import TruthF._

object TruthTracker extends TruthFunctions[Mu[TruthF]] {

  type Truth = Mu[TruthF]

  private type DefinitionName = String

  private type LatestDefinition = String

  private type Definitions = List[(DefinitionName, LatestDefinition)]

  private type Evaluation = Boolean

  val algebra: Algebra[TruthF, (LatestDefinition, Definitions, Evaluation)] = {
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

    case Define(name, (latest, definitions, p)) =>
      (name, (name, latest) :: definitions, p)
  }
}
