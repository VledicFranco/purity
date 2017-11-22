package purity.http4s

import cats.effect.IO
import org.http4s.client.blaze.PooledHttp1Client
import org.http4s.client.{Client, RequestKey}

/**
  * Intended to be mixed in services which are dependencies.
  * The maxTotalConnections and maxConnectionsPerRequestKey properties can be overriden to configure the http client.
  * See [[HttpClient]] documentation, that is the client the services should be using.
  */
trait HttpClientConfig {

  protected val maxTotalConnections: Int = 32

  protected val maxConnectionsPerRequestKey: RequestKey ⇒ Int = { _ ⇒ 32 }

  private val clientPool: Client[IO] = PooledHttp1Client[IO](
    maxTotalConnections = maxTotalConnections,
    maxConnectionsPerRequestKey = maxConnectionsPerRequestKey
  )

  protected val client: HttpClient = HttpClient(clientPool)

  def shutDownClientPool: IO[Unit] = clientPool.shutdown
}


