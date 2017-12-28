package purity.config

import com.typesafe.config.Config

trait ConfigContainer {

  def config: Config
}
