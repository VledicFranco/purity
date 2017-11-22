package purity.http4s

import io.circe.{DecodingFailure, Json}
import org.http4s.{Status, Uri}

import scala.util.control.NoStackTrace

/**
  * Exceptions to be used to fail IO monads when requesting other services. They add more data about the failure that
  * can be used for logging or creating more sensible responses to the client.
  */
sealed trait UnexpectedServiceResponse extends RuntimeException with NoStackTrace with Product with Serializable

/** IO.raiseError this when the service responded without the right content type or the data was corrupt. */
final case class ServiceUnexpectedEntity(uri: Uri, message: String) extends UnexpectedServiceResponse

/** IO.raiseError this when the service responded with not a successful status. */
final case class ServiceUnexpectedStatus(uri: Uri, json: Json, status: Status) extends UnexpectedServiceResponse

/** IO.raiseError this when the service responded with a different json than the one expected by the circe decoder. */
final case class ServiceUnexpectedModel(uri: Uri, json: Json, failure: DecodingFailure) extends UnexpectedServiceResponse


