package purity.script

import cats.effect._
import cats.implicits._
import cats.{Functor, Monad, MonadError}
import purity.logging.{LogLine, LoggerContainer}
import purity.script.ScriptT.Definition

/**
 * This represents a program with dependencies D, domain failures E, and which produces an A, while side effects should
 * be handled by F. Also it accumulates logging.
 *
 * One may see this data type as a handcrafted `ReaderT[WriterT[EitherT[F, E, ?], LogLine], D, A]` or a way to compose
 * functions with this signature: `D => F[(List[LogLine], Either[E, A])]`
 *
 * Use functions provided by the ScriptDSL type class to create and compose Script programs.
 *
 * Note that domain failures are supposed to model what can possible go wrong within the domain of the function, lower
 * level exceptions (like network failure) should be handled within the F[_] that the function `run` returns.
 *
 * @param definition function.
 * @tparam F should be a monad that handles async, like Future, IO, Task, or Streams.
 * @tparam D dependencies of the program.
 * @tparam E domain failures of the program.
 * @tparam A value that the program produces.
 */
case class ScriptT[F[+_], -D, +E, +A](definition: Definition[F, D, E, A]) extends ScriptDsl[F] {

  def map[B](f: A => B)(implicit F: Functor[F]): ScriptT[F, D, E, B] =
    ScriptT[F, D, E, B](d => definition(d).map(_.map(f)))

  def flatMap[B, DD <: D, EE >: E](f: A => ScriptT[F, DD, EE, B])(implicit M: Monad[F]): ScriptT[F, DD, EE, B] =
    ScriptT[F, DD, EE, B](d => definition(d).flatMap {
      case Left(e) => M.pure(Left(e))
      case Right(a) => f(a).definition(d)
    })

  /**
   * A normal functor map over the errors E. Useful when composing with another script that has different errors but
   * which you require it's produced value.
   */
  def leftMap[E2](f: E => E2)(implicit F: Functor[F]): ScriptT[F, D, E2, A] =
    ScriptT[F, D, E2, A](d => definition(d).map(_.leftMap(f)))

  /**
   * leftMap alias
   */
  def mapFailure[E2](f: E => E2)(implicit F: Functor[F]): ScriptT[F, D, E2, A] =
    this.leftMap(f)

  /**
   * One may see this as the flatMap of mapFailure (mapFailure creates a Functor, handleFailureWith creates a Monad)
   *
   *  Useful for when your original failure E is a coproduct, and you wish to handle just 1 or 2 errors but still keep
   *  the others. i.e:
   *  {{{
   *  import purity.script.io._
   *  val failed: Script[D, Either[Int, String], String] = fail(Left(1))
   *  val handled: Script[D, String, String] = failed.recover {
   *    case Left(_) => Script.pure("The Int failure is now ok")
   *    case Right(e) => Script.fail(e) // The String failure is still a failure
   *  }
   *  }}}
   */
  def recoverFailure[E2, DD <: D, AA >: A](f: E => ScriptT[F, DD, E2, AA])(implicit M: Monad[F]): ScriptT[F, DD, E2, AA] =
    ScriptT[F, DD, E2, AA](d => M.flatMap(definition(d)) {
      case Right(a) => M.pure(Right(a))
      case Left(e) => f(e).definition(d)
    })

  def recoverError[DD <: D, EE >: E, AA >: A](f: Throwable => ScriptT[F, DD, EE, AA])(implicit M: MonadError[F, Throwable]): ScriptT[F, DD, EE, AA] =
    ScriptT[F, DD, EE, AA](d => M.attempt(definition(d)).flatMap {
      case Right(ea) => M.pure(ea)
      case Left(e) => f(e).definition(d)
    })

  /**
   * Injects the required dependencies from an upper level container.
   *  {{{
   *  case class AkkaD(system: ActorSystem)
   *  case class Dependencies(akka: AkkaD, uri: String)
   *
   *  val ping: Script[AkkaD, Nothing, Pong] = ...
   *
   *  val restOfProgram: Script[Dependencies, Nothing, Pong] = for {
   *    pong <- ping.inject[Dependencies](_.akka)
   *  } yield pong
   *  }}}
   */
  def contramap[D2](di: D2 => D): ScriptT[F, D2, E, A] =
    ScriptT[F, D2, E, A](d2 => definition(di(d2)))

  /**
   * contramap alias
   */
  def inject[D2](di: D2 => D): ScriptT[F, D2, E, A] =
    this.contramap(di)

  /**
   * Adds a log line if the Script's domain failure E has failed.
   */
  def logFailure(f: E => LogLine)(implicit M: Effect[F]): ScriptT[F, D with LoggerContainer[F], E, A] =
    recoverFailure(e => log.logline(f(e)).flatMap(_ => raiseFailure(e)))

  /**
    * Adds a log line if the Script's lower F has failed.
    */
  def logError(f: Throwable => LogLine)(implicit M: Effect[F]): ScriptT[F, D with LoggerContainer[F], E, A] =
    for {
      loggerContainer <- dependencies[LoggerContainer[F]]
      a <- ScriptT[F, D with LoggerContainer[F], E, A](d => M.handleErrorWith(definition(d)) { e =>
        loggerContainer.logger.log(f(e)) *> M.raiseError(e)
      })
    } yield a

  /**
   * Adds the promised dependencies, and removes the failures by adding a function
   * that will handle them. Produces an F that can finally be executed.
   *
   * @param dependencies required for the computation.
   * @param onFailure handler function.
   * @param onSuccess handler function.
   * @tparam B type which already contains an answer to domain failures, for example the `Result` data type from an http library.
   * @return an F monad error with the result.
   */
  def fold[B](dependencies: D, onFailure: E => B, onSuccess: A => B)(implicit M: Monad[F]): F[B] =
    M.map(definition(dependencies)) {
      case Left(e) => onFailure(e)
      case Right(a) => onSuccess(a)
    }

  /** Alias for fold */
  def run[B](dependencies: D, onFailure: E => B, onSuccess: A => B)(implicit F: Monad[F]): F[B] =
    this.fold(dependencies, onFailure, onSuccess)

  /** Same as `run` but the failure and success handlers return an F instead of a pure value. (Like a map vs flatMap) */
  def foldF[B](dependencies: D, onFailure: E => F[B], onSuccess: A => F[B])(implicit M: Monad[F]): F[B] =
    M.flatMap(definition(dependencies)) {
      case Left(e) => onFailure(e)
      case Right(a) => onSuccess(a)
    }

  /** Alias for foldF */
  def runF[B](dependencies: D, onFailure: E => F[B], onSuccess: A => F[B])(implicit F: Monad[F]): F[B] =
    this.runF(dependencies, onFailure, onSuccess)
}

object ScriptT extends ScriptTInstances {

  type Definition[F[+_], -D, +E, +A] = D => F[Either[E, A]]
}

private[purity] trait ScriptTInstances extends ScriptTInstances0 {

  implicit def stdMonadForScript[F[+_], D, E](implicit F0: Monad[F]): Monad[ScriptT[F, D, E, ?]] =
    new ScriptTMonad[F, D, E] {
      override implicit val F: Monad[F] = F0
    }
}

private[purity] trait ScriptTInstances0 {

  implicit def stdMonadErrorForScript[F[+_], D, E](implicit F0: MonadError[F, Throwable]): MonadError[ScriptT[F, D, E, ?], E] =
    new ScriptTMonadError[F, D, E] {
      override implicit val F: MonadError[F, Throwable] = F0
    }

  implicit def stdSyncForScript[F[+_], D, E](implicit F0: Sync[F]): Sync[ScriptT[F, D, E, ?]] =
    new ScriptTSync[F, D, E] {
      override implicit val F: Sync[F] = F0
    }

  implicit def stdLiftIOForScript[F[+_], D, E](implicit F0: LiftIO[F], F1: Functor[F]): LiftIO[ScriptT[F, D, E, ?]] =
    new ScriptTLiftIO[F, D, E] {
      override implicit val F: LiftIO[F] = F0
      override implicit val FF: Functor[F] = F1
    }

  implicit def stdAsyncForScript[F[+_], D, E](implicit F0: Async[F]): Async[ScriptT[F, D, E, ?]] =
    new ScriptTAsync[F, D, E] {
      override implicit val F: Async[F] = F0
      override implicit val FF: Functor[F] = F0
    }

  implicit def stdEffectForScript[F[+_], D](implicit F0: Effect[F], D0: D): Effect[ScriptT[F, D, Throwable, ?]] =
    new ScriptTEffect[F, D] {
      override implicit val F: Effect[F] = F0
      override implicit val FF: Functor[F] = F0
      override implicit val Dependencies: D = D0
    }
}

private[purity] trait ScriptTFunctor[F[+_], D, E] extends Functor[ScriptT[F, D, E, ?]] {

  implicit val F: Functor[F]

  override def map[A, B](fa: ScriptT[F, D, E, A])(f: A => B): ScriptT[F, D, E, B] = fa map f
}

private[purity] trait ScriptTMonad[F[+_], D, E] extends Monad[ScriptT[F, D, E, ?]] {

  implicit val F: Monad[F]

  def flatMap[A, B](fa: ScriptT[F, D, E, A])(f: (A) => ScriptT[F, D, E, B]): ScriptT[F, D, E, B] =
    fa.flatMap(f)

  def tailRecM[A, B](a: A)(f: (A) => ScriptT[F, D, E, Either[A, B]]): ScriptT[F, D, E, B] =
    ScriptT[F, D, E, B](d => F.tailRecM(a)(a0 =>
      F.map(f(a0).definition(d)) {
        case Left(e) => Right(Left(e))
        case Right(Left(a1)) => Left(a1)
        case Right(Right(b)) => Right(Right(b))
      }
    ))

  def pure[A](x: A): ScriptT[F, D, E, A] =
    ScriptT[F, Any, Nothing, A](_ => F.pure(Right(x)))
}

private[purity] trait ScriptTMonadError[F[+_], D, E] extends MonadError[ScriptT[F, D, E, ?], E] with ScriptTMonad[F, D, E] {

  override implicit val F: MonadError[F, Throwable]

  override def raiseError[A](e: E): ScriptT[F, D, E, A] =
    ScriptT[F, Any, E, Nothing](_ => F.pure(Left(e)))

  override def handleErrorWith[A](fa: ScriptT[F, D, E, A])(f: (E) => ScriptT[F, D, E, A]): ScriptT[F, D, E, A] =
    fa.recoverFailure(f)
}


private[purity] trait ScriptTSync[F[+_], D, E] extends Sync[ScriptT[F, D, E, ?]] with ScriptTMonad[F, D, E] with ScriptDsl[F] {

  override implicit val F: Sync[F]

  override def suspend[A](thunk: =>ScriptT[F, D, E, A]): ScriptT[F, D, E, A] =
    ScriptT[F, D, E, A](d ⇒ F.suspend(thunk.definition(d)))

  override def raiseError[A](e: Throwable): ScriptT[F, D, E, A] =
    liftF(F.raiseError(e))

  override def handleErrorWith[A](fa: ScriptT[F, D, E, A])(f: Throwable => ScriptT[F, D, E, A]): ScriptT[F, D, E, A] =
    ScriptT[F, D, E, A](d ⇒ F.handleErrorWith(fa.definition(d))(f.andThen(_.definition(d))))
}

private[purity] trait ScriptTLiftIO[F[+_], D, E] extends LiftIO[ScriptT[F, D, E, ?]] with ScriptDsl[F] {

  implicit val F: LiftIO[F]
  implicit val FF: Functor[F]

  override def liftIO[A](ioa: IO[A]): ScriptT[F, D, E, A] =
    liftF(F.liftIO(ioa))
}

private[purity] trait ScriptTAsync[F[+_], D, E] extends Async[ScriptT[F, D, E, ?]] with ScriptTSync[F, D, E] with ScriptTLiftIO[F, D, E] {

  override implicit val F: Async[F]

  override def async[A](k: (Either[Throwable, A] => Unit) => Unit): ScriptT[F, D, E, A] =
    liftF(F.async(k))
}

private[purity] trait ScriptTEffect[F[+_], D] extends Effect[ScriptT[F, D, Throwable, ?]] with ScriptTAsync[F, D, Throwable] {

  override implicit val F: Effect[F]

  implicit val Dependencies: D

  override def runAsync[A](fa: ScriptT[F, D, Throwable, A])(cb: Either[Throwable, A] => IO[Unit]): IO[Unit] =
    F.runAsync(fa.foldF(Dependencies, F.raiseError, F.pure)(F))(cb)
}