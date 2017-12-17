package purity

import cats.effect.IO
import purity.script.io.{Script, dependencies, find, log, script, scriptE}
import purity.logging.{ColorPrint, LogLevel, Logger}

object UserAuth0 {

  type UserId = Int

  case class User(email: String, age: Int)

  case class Credentials(email: String, password: String)

  case class Response(code: Int, body: String)

  /** Dependencies */

  trait DatabaseConfig {
    val databaseUri: String
    val databaseUsername: String
    val databasePassword: String
  }

  trait AuthenticationServiceConfig {
    val authServiceUrl: String
    val authServiceToken: String
  }

  /** Domain Failures */

  case class UserNotFound(email: UserId)

  case class WrongCredentials(credentials: Credentials)

  /** IO functions */

  /** Tries to fetch a user id by using an authentication service.
    *  Note that it requires some configuration for this to happen.
    *  The function returns a cats IO because it needs to access the network in order to use the authentication service.
    */
  def authenticationIO(credentials: Credentials, config: AuthenticationServiceConfig): IO[Either[WrongCredentials, UserId]] =
    // Mock of the actual service call
    IO.pure(Right(1))

  def authentication(credentials: Credentials): Script[AuthenticationServiceConfig, WrongCredentials, UserId] =
    for {
      config <- dependencies[AuthenticationServiceConfig]
      _ <- log.info(s"Authenticating $credentials using service ${config.authServiceUrl}")
      userId <- scriptE(authenticationIO(credentials, config))
    } yield userId

  /** Tries to fetch a user from the database.
    *  Note that it requires some configuration as well.
    *  The function returns also a cats IO because querying the database accesses the network.
    */
  def queryUserIO(userId: UserId, config: DatabaseConfig): IO[Option[User]] =
    // Mock of the actual query
    IO.pure(Some(User("franco@lambda.org", 27)))

  def queryUser(userId: UserId): Script[DatabaseConfig, UserNotFound, User] =
    for {
      config <- dependencies[DatabaseConfig]
      _ <- log.trace(s"Querying for user $userId in database ${config.databaseUri}")
      maybeUser <- script(queryUserIO(userId, config))
      user <- find(UserNotFound(userId))(maybeUser)
    } yield user

  /** Our highly expressive Script programs. */

  type GetUserConfiguration = AuthenticationServiceConfig with DatabaseConfig

  type GetUserFailure = Either[WrongCredentials, UserNotFound]

  def authenticatedUser(credentials: Credentials): Script[GetUserConfiguration, GetUserFailure, User] =
    for {
      userId <- authentication(credentials).mapFailure(Left.apply)
      user <- queryUser(userId).mapFailure(Right.apply)
    } yield user

  def fetchUserAge(credentials: Credentials): Script[GetUserConfiguration, GetUserFailure, Int] =
    authenticatedUser(credentials).map(_.age)

  /** Finally provide the dependencies, logging, and failure handler in a function that handles the communication layer
    * (Probably for your http library).
    */
  def ageRequest(credentials: Credentials): IO[Response] =
    fetchUserAge(credentials).fold(config, logger, failureHandler, successHandler)

  val config: GetUserConfiguration =
    new DatabaseConfig with AuthenticationServiceConfig {
      val databaseUri: String = "some:uri"
      val databasePassword: String = "some-password"
      val databaseUsername: String = "root"
      val authServiceUrl: String = "auth.service/auth"
      val authServiceToken: String = "1234567890"
    }

  val logger: Logger = ColorPrint(LogLevel.AllLevel)

  def failureHandler(failure: GetUserFailure): Response =
    failure match {
      case Left(wrongCredentials) =>
        Response(400, s"Authentication with user ${wrongCredentials.credentials.email} failed")
      case Right(userNotFound) =>
        Response(500, s"Could not find user ${userNotFound.email} in the database")
    }

  def successHandler(age: Int): Response =
    Response(200, s"Here is the age you were looking for! :: $age")
}
