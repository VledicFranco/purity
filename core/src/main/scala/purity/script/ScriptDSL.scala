package purity.script

import cats.{ Applicative, Functor, Monad, MonadError }
import cats.implicits._
import purity.logging.LogLine._
import purity.logging.{ LogLine, LoggerFunction }
import purity.script.ScriptT.ExceptionWithLogs

trait ScriptDSL[F[+_]] {

  type Script[-D, +E, +A] = ScriptT[F, D, E, A]

  def map[A, B, D, E](sa: ScriptT[F, D, E, A])(f: A ⇒ B)(implicit F: Functor[F]): ScriptT[F, D, E, B] = {
    val FG = F.compose[(List[LogLine], ?)]
    ScriptT[F, D, E, B](d ⇒ FG.map(sa.definition(d))(_.bimap(identity, f)))(this)
  }

  def pure[A](a: A)(implicit F: Applicative[F]): ScriptT[F, Any, Nothing, A] =
    ScriptT[F, Any, Nothing, A](_ ⇒ F.pure((Nil, Right(a))))(this)

  def flatMap[A, B, D, E](sa: ScriptT[F, D, E, A])(f: (A) ⇒ ScriptT[F, D, E, B])(implicit M: MonadError[F, Throwable]): ScriptT[F, D, E, B] =
    ScriptT[F, D, E, B](d ⇒ M.flatMap(sa.definition(d)) {
      case (logs, Left(e)) ⇒
        M.pure((logs, Left(e)))
      case (logs, Right(a)) ⇒
        val fb = f(a).definition(d)
        M.map(fb) {
          case (moreLogs, ea) ⇒ (logs ++ moreLogs, ea)
        }.recoverWith {
          case ExceptionWithLogs(moreLogs, e) ⇒ M.raiseError(ExceptionWithLogs(logs ++ moreLogs, e))
          case e: Throwable                   ⇒ M.raiseError(ExceptionWithLogs(logs, e))
        }
    })(this)

  def tailRecM[A, B, D, E](a: A)(f: (A) ⇒ ScriptT[F, D, E, Either[A, B]])(implicit M: Monad[F]): ScriptT[F, D, E, B] =
    ScriptT[F, D, E, B](d ⇒ M.tailRecM(a)(a0 ⇒ M.map(f(a0).definition(d)) {
      case (logs, Left(e))         ⇒ Right((logs, Left(e)))
      case (_, Right(Left(a1)))    ⇒ Left(a1)
      case (logs, Right(Right(b))) ⇒ Right((logs, Right(b)))
    }))(this)

  def raiseError[E](e: E)(implicit F: Applicative[F]): ScriptT[F, Any, E, Nothing] =
    ScriptT[F, Any, E, Nothing](_ ⇒ F.pure((Nil, Left(e))))(this)

  def raiseError[E](e: E, logLine: LogLine)(implicit F: Applicative[F]): ScriptT[F, Any, E, Nothing] =
    ScriptT[F, Any, E, Nothing](_ ⇒ F.pure((List(logLine), Left(e))))(this)

  def fail[E](e: E)(implicit F: Applicative[F]): ScriptT[F, Any, E, Nothing] =
    raiseError(e)

  def fail[E](e: E, logLine: LogLine)(implicit F: Applicative[F]): ScriptT[F, Any, E, Nothing] =
    raiseError(e, logLine)

  def find[E, A](e: E)(opt: Option[A])(implicit F: Applicative[F]): ScriptT[F, Any, E, A] =
    opt.fold[ScriptT[F, Any, E, A]](fail(e))(pure(_))

  def find[E, A](opt: Either[E, A])(implicit F: Applicative[F]): ScriptT[F, Any, E, A] =
    opt.fold[ScriptT[F, Any, E, A]](fail(_), pure(_))

  def handleErrorWith[E, E2, D, A](sa: ScriptT[F, D, E, A])(f: (E) ⇒ ScriptT[F, D, E2, A])(implicit M: Monad[F]): ScriptT[F, D, E2, A] =
    ScriptT[F, D, E2, A](d ⇒ M.flatMap(sa.definition(d)) {
      case (logs, Right(a)) ⇒ M.pure((logs, Right(a)))
      case (logs, Left(e)) ⇒ M.map(f(e).definition(d)) {
        case (moreLogs, ea) ⇒ (logs ++ moreLogs, ea)
      }
    })(this)

  def recover[E, E2, D, A](sa: ScriptT[F, D, E, A])(f: (E) ⇒ ScriptT[F, D, E2, A])(implicit M: Monad[F]): ScriptT[F, D, E2, A] =
    handleErrorWith(sa)(f)

  def leftMap[E, E2, D, A](sa: ScriptT[F, D, E, A])(f: E ⇒ E2)(implicit F: Functor[F]): ScriptT[F, D, E2, A] = {
    val FG = F.compose[(List[LogLine], ?)]
    ScriptT[F, D, E2, A](d ⇒ FG.map(sa.definition(d))(_.bimap(f, identity)))(this)
  }

  def mapFailure[E, E2, D, A](sa: ScriptT[F, D, E, A])(f: E ⇒ E2)(implicit F: Functor[F]): ScriptT[F, D, E2, A] =
    leftMap(sa)(f)

  def contramap[D2, D, E, A](sa: ScriptT[F, D, E, A])(di: D2 ⇒ D): ScriptT[F, D2, E, A] =
    ScriptT[F, D2, E, A](d2 ⇒ sa.definition(di(d2)))(this)

  def inject[D2, D, E, A](sa: ScriptT[F, D, E, A])(di: D2 ⇒ D): ScriptT[F, D2, E, A] =
    contramap(sa)(di)

  def unit(implicit F: Applicative[F]): ScriptT[F, Any, Nothing, Unit] =
    ScriptT[F, Any, Nothing, Unit](_ ⇒ F.pure((Nil, Right(()))))(this)

  def start(implicit F: Applicative[F]): ScriptT[F, Any, Nothing, Unit] = unit

  def ok[E](implicit F: Applicative[F]): ScriptT[F, Any, E, Unit] = unit

  def liftF[A](sa: F[A])(implicit F: Functor[F]): ScriptT[F, Any, Nothing, A] =
    ScriptT[F, Any, Nothing, A](_ ⇒ F.map(sa)(a ⇒ (Nil, Right(a))))(this)

  def liftFE[E, A](fae: F[Either[E, A]])(implicit F: Functor[F]): ScriptT[F, Any, E, A] =
    ScriptT[F, Any, E, A](_ ⇒ F.map(fae)(ea ⇒ (Nil, ea)))(this)

  def script[A](sa: F[A])(implicit F: Functor[F]): ScriptT[F, Any, Nothing, A] =
    liftF(sa)

  def scriptE[E, A](fae: F[Either[E, A]])(implicit F: Functor[F]): ScriptT[F, Any, E, A] =
    liftFE(fae)

  def fold[A, B, D, E](sa: ScriptT[F, D, E, A])(dependencies: D, logger: LoggerFunction, onFailure: E ⇒ B, onSuccess: A ⇒ B)(implicit M: MonadError[F, Throwable]): F[B] =
    M.map(sa.definition(dependencies)) {
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

  def run[A, B, D, E](sa: ScriptT[F, D, E, A])(dependencies: D, logger: LoggerFunction, onFailure: E ⇒ B, onSuccess: A ⇒ B)(implicit M: MonadError[F, Throwable]): F[B] =
    fold(sa)(dependencies, logger, onFailure, onSuccess)

  def foldF[A, B, D, E](sa: ScriptT[F, D, E, A])(dependencies: D, logger: LoggerFunction, onFailure: E ⇒ F[B], onSuccess: A ⇒ F[B])(implicit M: MonadError[F, Throwable]): F[B] =
    M.flatMap(sa.definition(dependencies)) {
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

  def runF[A, B, D, E](sa: ScriptT[F, D, E, A])(dependencies: D, logger: LoggerFunction, onFailure: E ⇒ F[B], onSuccess: A ⇒ F[B])(implicit M: MonadError[F, Throwable]): F[B] =
    foldF(sa)(dependencies, logger, onFailure, onSuccess)

  def dependencies[D](implicit F: Applicative[F]): ScriptT[F, D, Nothing, D] =
    ScriptT[F, D, Nothing, D](d ⇒ F.pure((Nil, Right(d))))(this)

  def log(logLine: LogLine)(implicit F: Applicative[F]): ScriptT[F, Any, Nothing, Unit] =
    ScriptT[F, Any, Nothing, Unit](_ ⇒ F.pure((List(logLine), Right(()))))(this)

  def debug(message: String)(implicit F: Applicative[F]): ScriptT[F, Any, Nothing, Unit] =
    log(Debug(message))

  def debug(e: Throwable)(implicit F: Applicative[F]): ScriptT[F, Any, Nothing, Unit] =
    log(Debug(e.getMessage, Some(e)))

  def debug(message: String, e: Throwable)(implicit F: Applicative[F]): ScriptT[F, Any, Nothing, Unit] =
    log(Debug(message, Some(e)))

  def error(message: String)(implicit F: Applicative[F]): ScriptT[F, Any, Nothing, Unit] =
    log(Error(message, None))

  def error(e: Throwable)(implicit F: Applicative[F]): ScriptT[F, Any, Nothing, Unit] =
    log(Error(e.getMessage, Some(e)))

  def error(message: String, e: Throwable)(implicit F: Applicative[F]): ScriptT[F, Any, Nothing, Unit] =
    log(Error(message, Some(e)))

  def fatal(message: String)(implicit F: Applicative[F]): ScriptT[F, Any, Nothing, Unit] =
    log(Fatal(message, None))

  def fatal(e: Throwable)(implicit F: Applicative[F]): ScriptT[F, Any, Nothing, Unit] =
    log(Fatal(e.getMessage, Some(e)))

  def fatal(message: String, e: Throwable)(implicit F: Applicative[F]): ScriptT[F, Any, Nothing, Unit] =
    log(Fatal(message, Some(e)))

  def info(message: String)(implicit F: Applicative[F]): ScriptT[F, Any, Nothing, Unit] =
    log(Info(message))

  def info(e: Throwable)(implicit F: Applicative[F]): ScriptT[F, Any, Nothing, Unit] =
    log(Info(e.getMessage, Some(e)))

  def info(message: String, e: Throwable)(implicit F: Applicative[F]): ScriptT[F, Any, Nothing, Unit] =
    log(Info(message, Some(e)))

  def off(message: String)(implicit F: Applicative[F]): ScriptT[F, Any, Nothing, Unit] =
    log(Off(message))

  def off(e: Throwable)(implicit F: Applicative[F]): ScriptT[F, Any, Nothing, Unit] =
    log(Off(e.getMessage, Some(e)))

  def off(message: String, e: Throwable)(implicit F: Applicative[F]): ScriptT[F, Any, Nothing, Unit] =
    log(Off(message, Some(e)))

  def trace(message: String)(implicit F: Applicative[F]): ScriptT[F, Any, Nothing, Unit] =
    log(Trace(message))

  def trace(e: Throwable)(implicit F: Applicative[F]): ScriptT[F, Any, Nothing, Unit] =
    log(Trace(e.getMessage, Some(e)))

  def trace(message: String, e: Throwable)(implicit F: Applicative[F]): ScriptT[F, Any, Nothing, Unit] =
    log(Trace(message, Some(e)))

  def warn(message: String)(implicit F: Applicative[F]): ScriptT[F, Any, Nothing, Unit] =
    log(Warn(message))

  def warn(e: Throwable)(implicit F: Applicative[F]): ScriptT[F, Any, Nothing, Unit] =
    log(Warn(e.getMessage, Some(e)))

  def warn(message: String, e: Throwable)(implicit F: Applicative[F]): ScriptT[F, Any, Nothing, Unit] =
    log(Warn(message, Some(e)))

  def logFailure[D, E, A](sa: ScriptT[F, D, E, A])(f: E ⇒ LogLine)(implicit M: Functor[F]): ScriptT[F, D, E, A] =
    ScriptT[F, D, E, A](d ⇒ M.map(sa.definition(d)) {
      case (logs, Left(e))  ⇒ (logs :+ f(e), Left(e))
      case (logs, Right(a)) ⇒ (logs, Right(a))
    })(this)

  def logError[D, E, A](sa: ScriptT[F, D, E, A])(f: Throwable ⇒ LogLine)(implicit M: MonadError[F, Throwable]): ScriptT[F, D, E, A] =
    ScriptT[F, D, E, A] { d ⇒
      val fa = sa.definition(d)
      fa.recoverWith {
        case ExceptionWithLogs(logs, e) ⇒ M.raiseError(ExceptionWithLogs(logs :+ f(e), e))
        case e: Throwable               ⇒ M.raiseError(ExceptionWithLogs(List(f(e)), e))
      }
    }(this)
}
