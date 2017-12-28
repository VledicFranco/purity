package purity.http4s

import purity.config.ConfigContainer
import purity.logging.LoggerContainer

trait ServiceTools[F[+_]]
  extends JsonClientContainer[F]
    with LoggerContainer[F]
    with ConfigContainer

