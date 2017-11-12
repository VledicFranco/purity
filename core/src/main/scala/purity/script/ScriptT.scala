package purity.script

import cats.{Functor, Monad, MonadError}
import cats.implicits._
import purity.logging.{LogLine, LoggerFunction}
import purity.script.ScriptT.{Definition, ExceptionWithLogs}

import scala.util.control.NoStackTrace

/**
 * This represents a program with dependencies D, domain failures E, and which produces an A, while side effects should
 * be handled by F. Also it accumulates logging.
 *
 * One may see this data type as a handcrafted `ReaderT[WriterT[EitherT[F, E, ?], LogLine], D, A]` or a way to compose
 * functions with this signature: `D ⇒ F[(List[LogLine], Either[E, A])]`
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

  def map[B](f: A ⇒ B)(implicit F: Functor[F]): ScriptT[F, D, E, B] = {
    val FG = F.compose[(List[LogLine], ?)]
    ScriptT[F, D, E, B](d ⇒ FG.map(definition(d))(_.bimap(identity, f)))
  }

  def flatMap[B, DD <: D, EE >: E](f: A ⇒ ScriptT[F, DD, EE, B])(implicit M: MonadError[F, Throwable]): ScriptT[F, DD, EE, B] =
    ScriptT[F, DD, EE, B](d ⇒ M.flatMap(definition(d)) {
      case (logs, Left(e)) ⇒
        M.pure((logs, Left(e)))
      case (logs, Right(a)) ⇒
        val fb = f(a).definition(d)
        M.map(fb) {
          case (moreLogs, ea) ⇒ (logs ++ moreLogs, ea)
        }.recoverWith {
          case ExceptionWithLogs(moreLogs, e) ⇒ M.raiseError(ExceptionWithLogs(logs ++ moreLogs, e))
          case e: Throwable ⇒ M.raiseError(ExceptionWithLogs(logs, e))
        }
    })

  /**
   * A normal functor map over the errors E. Useful when composing with another script that has different errors but
   * which you require it's produced value.
   */
  def leftMap[E2](f: E ⇒ E2)(implicit F: Functor[F]): ScriptT[F, D, E2, A] = {
    val FG = F.compose[(List[LogLine], ?)]
    ScriptT[F, D, E2, A](d ⇒ FG.map(definition(d))(_.bimap(f, identity)))
  }

  /**
   * leftMap alias
   */
  def mapFailure[E2](f: E ⇒ E2)(implicit F: Functor[F]): ScriptT[F, D, E2, A] =
    this.leftMap(f)

  /**
   * Just like [[cats.MonadError]] `handleErrorWith`, but does a mapping on the failure type through the process.
   * Also one may see this as the flatMap of mapFailure (mapFailure creates a Functor, recover creates a Monad)
   *
   *  Useful for when your original failure E is a coproduct, and you wish to handle just 1 or 2 errors but still keep
   *  others. i.e:
   *  {{{
   *  import purity.script.io._
   *  val failed: Script[D, Either[Int, String], String] = fail(Left(1))
   *  val handled: Script[D, String, String] = failed.recover {
   *    case Left(_) => Script.pure("The Int failure is now ok")
   *    case Right(e) => Script.fail(e) // The String failure is still a failure
   *  }
   *  }}}
   */
  def handleErrorWith[E2, DD <: D, AA >: A](f: (E) ⇒ ScriptT[F, DD, E2, AA])(implicit M: Monad[F]): ScriptT[F, DD, E2, AA] =
    ScriptT[F, DD, E2, AA](d ⇒ M.flatMap(definition(d)) {
      case (logs, Right(a)) ⇒ M.pure((logs, Right(a)))
      case (logs, Left(e)) ⇒ M.map(f(e).definition(d)) {
        case (moreLogs, ea) ⇒ (logs ++ moreLogs, ea)
      }
    })

  /**
   * handleErrorWith alias
   */
  def recover[E2, DD <: D, AA >: A](f: E ⇒ ScriptT[F, DD, E2, AA])(implicit M: Monad[F]): ScriptT[F, DD, E2, AA] =
    this.handleErrorWith(f)

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
  def contramap[D2](di: D2 ⇒ D): ScriptT[F, D2, E, A] =
    ScriptT[F, D2, E, A](d2 ⇒ definition(di(d2)))

  /**
   * contramap alias
   */
  def inject[D2](di: D2 ⇒ D): ScriptT[F, D2, E, A] =
    this.contramap(di)

  def logFailure(f: E ⇒ LogLine)(implicit M: Functor[F]): ScriptT[F, D, E, A] =
    ScriptT[F, D, E, A](d ⇒ M.map(definition(d)) {
      case (logs, Left(e))  ⇒ (logs :+ f(e), Left(e))
      case (logs, Right(a)) ⇒ (logs, Right(a))
    })

  def logError(f: Throwable ⇒ LogLine)(implicit M: MonadError[F, Throwable]): ScriptT[F, D, E, A] =
    ScriptT[F, D, E, A] { d ⇒
      val fa = definition(d)
      fa.recoverWith {
        case ExceptionWithLogs(logs, e) ⇒ M.raiseError(ExceptionWithLogs(logs :+ f(e), e))
        case e: Throwable               ⇒ M.raiseError(ExceptionWithLogs(List(f(e)), e))
      }
    }

  /**
   * Adds the promised dependencies, runs a logger with the log lines, and removes the failures by adding a function
   * that will handle them. Produces an F that can finally be executed.
   *
   * @param dependencies required for the computation.
   * @param logger function with side effects that does the actual logging.
   * @param onFailure handler function.
   * @param onSuccess handler function.
   * @tparam B type which already contains an answer to domain failures, for example the `Result` data type from an http library.
   * @return an IO monad that still needs to be run and handled.
   */
  def fold[B](dependencies: D, logger: LoggerFunction, onFailure: E ⇒ B, onSuccess: A ⇒ B)(implicit M: MonadError[F, Throwable]): F[B] =
    M.map(definition(dependencies)) {
      case (logs, Left(e)) ⇒
        logs.foreach(logger.log); onFailure(e)
      case (logs, Right(a)) ⇒
        logs.foreach(logger.log); onSuccess(a)
    }.handleErrorWith {
      case ExceptionWithLogs(logs, e) ⇒
        logs.foreach(logger.log); M.raiseError(e)
      case e ⇒
        M.raiseError(e)
    }

  /** Alias for fold */
  def run[B](dependencies: D, logger: LoggerFunction, onFailure: E ⇒ B, onSuccess: A ⇒ B)(implicit F: MonadError[F, Throwable]): F[B] =
    this.fold(dependencies, logger, onFailure, onSuccess)

  /** Same as `run` but the failure and success handlers return an F instead of a pure value. (Like a map vs flatMap) */
  def foldF[B](dependencies: D, logger: LoggerFunction, onFailure: E ⇒ F[B], onSuccess: A ⇒ F[B])(implicit M: MonadError[F, Throwable]): F[B] =
    M.flatMap(definition(dependencies)) {
      case (logs, Left(e)) ⇒
        logs.foreach(logger.log); onFailure(e)
      case (logs, Right(a)) ⇒
        logs.foreach(logger.log); onSuccess(a)
    }.handleErrorWith {
      case ExceptionWithLogs(logs, e) ⇒
        logs.foreach(logger.log); M.raiseError(e)
      case e ⇒
        M.raiseError(e)
    }

  /** Alias for foldF */
  def runF[B](dependencies: D, logger: LoggerFunction, onFailure: E ⇒ F[B], onSuccess: A ⇒ F[B])(implicit F: MonadError[F, Throwable]): F[B] =
    this.runF(dependencies, logger, onFailure, onSuccess)
}

object ScriptT extends ScriptTInstances {

  type Definition[F[+_], -D, +E, +A] = D ⇒ F[(List[LogLine], Either[E, A])]

  private[purity] final case class ExceptionWithLogs(logs: List[LogLine], e: Throwable) extends RuntimeException with NoStackTrace with Product with Serializable
}

private[purity] trait ScriptTInstances {

  implicit def stdMonadErrorForScript[F[+_], D, E](implicit M: MonadError[F, Throwable]): MonadError[ScriptT[F, D, E, ?], E] = {
    new MonadError[ScriptT[F, D, E, ?], E] {
      override def flatMap[A, B](fa: ScriptT[F, D, E, A])(f: (A) ⇒ ScriptT[F, D, E, B]): ScriptT[F, D, E, B] =
        fa.flatMap(f)

      override def tailRecM[A, B](a: A)(f: (A) ⇒ ScriptT[F, D, E, Either[A, B]]): ScriptT[F, D, E, B] =
        ScriptT[F, D, E, B](d ⇒ M.tailRecM(a)(a0 ⇒ M.map(f(a0).definition(d)) {
          case (logs, Left(e))         ⇒ Right((logs, Left(e)))
          case (_, Right(Left(a1)))    ⇒ Left(a1)
          case (logs, Right(Right(b))) ⇒ Right((logs, Right(b)))
        }))

      override def pure[A](x: A): ScriptT[F, D, E, A] =
        ScriptT[F, Any, Nothing, A](_ ⇒ M.pure((Nil, Right(x))))

      override def raiseError[A](e: E): ScriptT[F, D, E, A] =
        ScriptT[F, Any, E, Nothing](_ ⇒ M.pure((Nil, Left(e))))

      override def handleErrorWith[A](fa: ScriptT[F, D, E, A])(f: (E) ⇒ ScriptT[F, D, E, A]): ScriptT[F, D, E, A] =
        fa.handleErrorWith(f)
    }
  }
}
