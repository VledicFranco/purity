package purity.http4s

import purity.config.ConfigContainer
import purity.script.ScriptDsl

trait JsonClientContainer[F[+_]] extends ScriptDsl[F] {

  def jsonClient: CantFail[ConfigContainer, JsonClient[F]]
}
