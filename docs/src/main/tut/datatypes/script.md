---
layout: docs
title:  "Script"
section: "data"
---

## Script

Script is a monad built from the ground up with the objective of handling the most common basic requirements of a
developers day to day programs. It is designed to be as easy as possible to use. It can be used to address
dependency injection, domain failure handling, low level error handling, IO, and mocking, all of these while being
highly compositional, referential transparent and type safe. Script is not a silver bullet, but hopefully it will be a
useful tool for several of your use cases.

### Small overview of the Script type

Script is in reality an implementation of `ScriptT[F[_], D, E, A]` (a monad transformer) where:

* `F[_]` is a monad that is able to handle IO or asynchronicity (Like Future, IO or Task).
* `D` represents some dependencies.
* `E` represents some domain failures.
* `A` represents a pure value.

Also Script accumulates log lines using the `purity.logging.LogLine` type.

For ease of use, purity provides two implementations of the ScriptT, one with `cats.effects.IO` and the other with
Scala's Future. The former (which is recommended over Futures) is located at `purity.script.io.Script`, and the latter
at `purity.script.future.Script`. Also the same packages contain all of the functions you will need.

#### A quick example in code

This is the full example that we will use in the next tutorials. Each part will be explained, modified with alternatives,
and improved upon! As well most of the code can be found in the `example` project folder within the repository of the
project.

```scala
import cats.effect.IO
import purity.script.io.{Script, dependencies, find, log, script, scriptE}
import purity.logging.{ColorPrint, LogLevel, LoggerFunction}

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

  val logger: LoggerFunction = ColorPrint(LogLevel.AllLevel)

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
```

#### For those familiar with monad transformers:

One may see Script as a a stack of a `ReaderT` (for dependencies) with `WriterT`
(for logs) with EitherT (for domain failures) and an `F[_]` like `IO` or `Future` (for IO, async and low level errors).
The differences between this two are:

1) Script provides several functions to add syntax and ease of use of this common stack
2) And most importantly, Script has variance annotations, leveraging Scala's subtyping system, we will see how this may
become powerful when composing dependencies.

### Difference between domain failures and low level errors

Domain failures are supposed to model what can possible go wrong within the "business" domain of the function, lower
level exceptions (like network failure) should be handled within the F[_] that the function `run` returns.