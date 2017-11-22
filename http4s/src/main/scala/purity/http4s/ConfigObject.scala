package purity.http4s

import com.typesafe.config.Config

/**
  * Mix in with dependencies that require to extract configuration from the application.conf file.
  */
trait ConfigObject {

  def config: Config
}


