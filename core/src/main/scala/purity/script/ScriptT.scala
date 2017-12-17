package purity.script

import cats.{Functor, Monad, MonadError}
import cats.implicits._
import purity.logging.{LogLine, LoggerFunction}
import purity.script.ScriptT.Definition

import scala.util.control.NoStackTrace

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
case class ScriptT[F[+_], -D, +E, +A](definition: Definition[F, D, E, A]) {

  def map[B](f: A => B)(implicit F: Functor[F]): ScriptT[F, D, E, B] = {
    val FG = F.compose[(List[LogLine], ?)]
    ScriptT[F, D, E, B](d => FG.map(definition(d))(_.bimap(identity, f)))
  }

  def flatMap[B, DD <: D, EE >: E](f: A => ScriptT[F, DD, EE, B])(implicit M: Monad[F]): ScriptT[F, DD, EE, B] =
    ScriptT[F, DD, EE, B](d => M.flatMap(definition(d)) {
      case (logs, Left(e)) =>
        M.pure((logs, Left(e)))
      case (logs, Right(a)) =>
        val fb = f(a).definition(d)
        M.map(fb) { case (moreLogs, ea) => (logs ++ moreLogs, ea) }
    })

  /**
   * A normal functor map over the errors E. Useful when composing with another script that has different errors but
   * which you require it's produced value.
   */
  def leftMap[E2](f: E => E2)(implicit F: Functor[F]): ScriptT[F, D, E2, A] = {
    val FG = F.compose[(List[LogLine], ?)]
    ScriptT[F, D, E2, A](d => FG.map(definition(d))(_.bimap(f, identity)))
  }

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
  def handleFailureWith[E2, DD <: D, AA >: A](f: E => ScriptT[F, DD, E2, AA])(implicit M: Monad[F]): ScriptT[F, DD, E2, AA] =
    ScriptT[F, DD, E2, AA](d => M.flatMap(definition(d)) {
      case (logs, Right(a)) => M.pure((logs, Right(a)))
      case (logs, Left(e)) => M.map(f(e).definition(d)) {
        case (moreLogs, ea) => (logs ++ moreLogs, ea)
      }
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
  def logFailure(f: E => LogLine)(implicit M: Functor[F]): ScriptT[F, D, E, A] =
    ScriptT[F, D, E, A](d => M.map(definition(d)) {
      case (logs, Left(e))  => (logs :+ f(e), Left(e))
      case (logs, Right(a)) => (logs, Right(a))
    })

  /**
   * Adds a log line if the inner F[_] monad error has failed.
   * TODO: Adapt to the new logging methodology
   */
  def logError(f: Throwable => LogLine)(implicit M: MonadError[F, Throwable]): ScriptT[F, D, E, A] =
    this

  /**
   * Adds the promised dependencies, runs a logger with the log lines, and removes the failures by adding a function
   * that will handle them. Produces an F that can finally be executed.
   *
   * @param dependencies required for the computation.
   * @param logger function with side effects that does the actual logging.
   * @param onFailure handler function.
   * @param onSuccess handler function.
   * @tparam B type which already contains an answer to domain failures, for example the `Result` data type from an http library.
   * @return an F monad error with the result.
   */
  def fold[B](dependencies: D, logger: LoggerFunction, onFailure: E => B, onSuccess: A => B)(implicit M: Monad[F]): F[B] =
    M.map(definition(dependencies)) {
      case (logs, Left(e)) =>
        logs.foreach(logger.log); onFailure(e)
      case (logs, Right(a)) =>
        logs.foreach(logger.log); onSuccess(a)
    }

  /** Alias for fold */
  def run[B](dependencies: D, logger: LoggerFunction, onFailure: E => B, onSuccess: A => B)(implicit F: Monad[F]): F[B] =
    this.fold(dependencies, logger, onFailure, onSuccess)

  /** Same as `run` but the failure and success handlers return an F instead of a pure value. (Like a map vs flatMap) */
  def foldF[B](dependencies: D, logger: LoggerFunction, onFailure: E => F[B], onSuccess: A => F[B])(implicit M: Monad[F]): F[B] =
    M.flatMap(definition(dependencies)) {
      case (logs, Left(e)) =>
        logs.foreach(logger.log); onFailure(e)
      case (logs, Right(a)) =>
        logs.foreach(logger.log); onSuccess(a)
    }

  /** Alias for foldF */
  def runF[B](dependencies: D, logger: LoggerFunction, onFailure: E => F[B], onSuccess: A => F[B])(implicit F: Monad[F]): F[B] =
    this.runF(dependencies, logger, onFailure, onSuccess)
}

object ScriptT extends ScriptTInstances {

  type Definition[F[+_], -D, +E, +A] = D => F[(List[LogLine], Either[E, A])]
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
    ScriptT[F, D, E, B](d => F.tailRecM((List.empty[LogLine], a))((a0: (List[LogLine], A)) =>
      F.map(f(a0._2).definition(d)) {
        case (logs, Left(e)) => Right((a0._1 ++ logs, Left(e)))
        case (logs, Right(Left(a1))) => Left((a0._1 ++ logs, a1))
        case (logs, Right(Right(b))) => Right((a0._1 ++ logs, Right(b)))
      }
    ))

  def pure[A](x: A): ScriptT[F, D, E, A] =
    ScriptT[F, Any, Nothing, A](_ => F.pure((Nil, Right(x))))
}

private[purity] trait ScriptTMonadError[F[+_], D, E] extends MonadError[ScriptT[F, D, E, ?], E] with ScriptTMonad[F, D, E] {

  implicit val F: MonadError[F, Throwable]

  override def raiseError[A](e: E): ScriptT[F, D, E, A] =
    ScriptT[F, Any, E, Nothing](_ => F.pure((Nil, Left(e))))

  override def handleErrorWith[A](fa: ScriptT[F, D, E, A])(f: (E) => ScriptT[F, D, E, A]): ScriptT[F, D, E, A] =
    fa.handleFailureWith(f)
}

