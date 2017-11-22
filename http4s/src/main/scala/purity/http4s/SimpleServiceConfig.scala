package purity.http4s

import org.http4s.{Uri, UriTemplate}

/**
  * Intended to be used inside services which are dependencies.
  * Basic configuration of a service. Can create the correct [[org.http4s.Uri]] of the endpoints.
  *
  * Note: This has impure methods that throw exceptions when trying to build the URLs and URIs, this is because this
  * objects should be instantiated when booting the server, and the server should NOT boot if the configured endpoints
  * are invalid.
  */
case class SimpleServiceConfig(https: Boolean, host: String, port: Int) {

  private val template = UriTemplate(
    authority = Some(Uri.Authority(host = Uri.RegName(host), port = Some(port))),
    scheme = Some({
      if (https) Uri.Scheme.https
      else Uri.Scheme.http
    })
  )

  /**
    * Root of the service path (e.g. https://example.com:9000) Since this should be valid as soon as the server boots,
    * this should throw an exception if the config is wrong.
    */
  val root: Uri = template.toUriIfPossible.fold[Uri](e ⇒ throw e, identity)

  /**
    * Builds an endpoint Uri with this service root and from a relative path string.
    * Since this should be valid as soon as the server boots, this should throw an exception if the endpoint is wrong.
    */
  def endpoint(path: String): Uri = {
    val p: Uri = Uri.fromString(path) match {
      case Left(e)    ⇒ throw new Exception("The configuration is wrong! Fix it before using it.\n" + e.getMessage)
      case Right(uri) ⇒ uri
    }
    root.resolve(p)
  }
}
