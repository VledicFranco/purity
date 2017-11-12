package purity.script

import cats.{ Applicative, Functor, Monad, MonadError }
import purity.logging.LogLine._
import purity.logging.{ LogLine, LoggerFunction }

trait ScriptDSL[F[+_]] {

  type Script[-D, +E, +A] = ScriptT[F, D, E, A]

  def map[A, B, D, E](sa: ScriptT[F, D, E, A])(f: A ⇒ B)(implicit F: Functor[F]): ScriptT[F, D, E, B] =
    sa.map(f)

  def pure[A](a: A)(implicit F: Applicative[F]): ScriptT[F, Any, Nothing, A] =
    ScriptT[F, Any, Nothing, A](_ ⇒ F.pure((Nil, Right(a))))

  def flatMap[A, B, D, E](sa: ScriptT[F, D, E, A])(f: (A) ⇒ ScriptT[F, D, E, B])(implicit M: MonadError[F, Throwable]): ScriptT[F, D, E, B] =
    sa.flatMap(f)

  def raiseError[E](e: E)(implicit F: Applicative[F]): ScriptT[F, Any, E, Nothing] =
    ScriptT[F, Any, E, Nothing](_ ⇒ F.pure((Nil, Left(e))))

  def raiseError[E](e: E, logLine: LogLine)(implicit F: Applicative[F]): ScriptT[F, Any, E, Nothing] =
    ScriptT[F, Any, E, Nothing](_ ⇒ F.pure((List(logLine), Left(e))))

  def fail[E](e: E)(implicit F: Applicative[F]): ScriptT[F, Any, E, Nothing] =
    this.raiseError(e)

  def fail[E](e: E, logLine: LogLine)(implicit F: Applicative[F]): ScriptT[F, Any, E, Nothing] =
    this.raiseError(e, logLine)

  def find[E, A](e: => E)(opt: Option[A])(implicit F: Applicative[F]): ScriptT[F, Any, E, A] =
    opt.fold[ScriptT[F, Any, E, A]](fail(e))(pure(_))

  def find[E, A](opt: Either[E, A])(implicit F: Applicative[F]): ScriptT[F, Any, E, A] =
    opt.fold[ScriptT[F, Any, E, A]](fail(_), pure(_))

  def handleErrorWith[E, E2, D, A](sa: ScriptT[F, D, E, A])(f: (E) ⇒ ScriptT[F, D, E2, A])(implicit M: Monad[F]): ScriptT[F, D, E2, A] =
    sa.handleErrorWith(f)

  def recover[E, E2, D, A](sa: ScriptT[F, D, E, A])(f: (E) ⇒ ScriptT[F, D, E2, A])(implicit M: Monad[F]): ScriptT[F, D, E2, A] =
    sa.handleErrorWith(f)

  def leftMap[E, E2, D, A](sa: ScriptT[F, D, E, A])(f: E ⇒ E2)(implicit F: Functor[F]): ScriptT[F, D, E2, A] =
    sa.leftMap(f)

  def mapFailure[E, E2, D, A](sa: ScriptT[F, D, E, A])(f: E ⇒ E2)(implicit F: Functor[F]): ScriptT[F, D, E2, A] =
    sa.leftMap(f)

  def contramap[D2, D, E, A](sa: ScriptT[F, D, E, A])(di: D2 ⇒ D): ScriptT[F, D2, E, A] =
    sa.contramap(di)

  def inject[D2, D, E, A](sa: ScriptT[F, D, E, A])(di: D2 ⇒ D): ScriptT[F, D2, E, A] =
    sa.contramap(di)

  def unit(implicit F: Applicative[F]): ScriptT[F, Any, Nothing, Unit] =
    ScriptT[F, Any, Nothing, Unit](_ ⇒ F.pure((Nil, Right(()))))

  def start(implicit F: Applicative[F]): ScriptT[F, Any, Nothing, Unit] =
    this.unit

  def ok[E](implicit F: Applicative[F]): ScriptT[F, Any, E, Unit] =
    this.unit

  def liftF[A](sa: F[A])(implicit F: Functor[F]): ScriptT[F, Any, Nothing, A] =
    ScriptT[F, Any, Nothing, A](_ ⇒ F.map(sa)(a ⇒ (Nil, Right(a))))

  def liftFE[E, A](fae: F[Either[E, A]])(implicit F: Functor[F]): ScriptT[F, Any, E, A] =
    ScriptT[F, Any, E, A](_ ⇒ F.map(fae)(ea ⇒ (Nil, ea)))

  def script[A](sa: F[A])(implicit F: Functor[F]): ScriptT[F, Any, Nothing, A] =
    this.liftF(sa)

  def scriptE[E, A](fae: F[Either[E, A]])(implicit F: Functor[F]): ScriptT[F, Any, E, A] =
    this.liftFE(fae)

  def fold[A, B, D, E](sa: ScriptT[F, D, E, A])(dependencies: D, logger: LoggerFunction, onFailure: E ⇒ B, onSuccess: A ⇒ B)(implicit M: MonadError[F, Throwable]): F[B] =
    sa.fold(dependencies, logger, onFailure, onSuccess)

  def run[A, B, D, E](sa: ScriptT[F, D, E, A])(dependencies: D, logger: LoggerFunction, onFailure: E ⇒ B, onSuccess: A ⇒ B)(implicit M: MonadError[F, Throwable]): F[B] =
    sa.fold(dependencies, logger, onFailure, onSuccess)

  def foldF[A, B, D, E](sa: ScriptT[F, D, E, A])(dependencies: D, logger: LoggerFunction, onFailure: E ⇒ F[B], onSuccess: A ⇒ F[B])(implicit M: MonadError[F, Throwable]): F[B] =
    sa.foldF(dependencies, logger, onFailure, onSuccess)

  def runF[A, B, D, E](sa: ScriptT[F, D, E, A])(dependencies: D, logger: LoggerFunction, onFailure: E ⇒ F[B], onSuccess: A ⇒ F[B])(implicit M: MonadError[F, Throwable]): F[B] =
    sa.foldF(dependencies, logger, onFailure, onSuccess)

  def dependencies[D](implicit F: Applicative[F]): ScriptT[F, D, Nothing, D] =
    ScriptT[F, D, Nothing, D](d ⇒ F.pure((Nil, Right(d))))

  def log(logLine: LogLine)(implicit F: Applicative[F]): ScriptT[F, Any, Nothing, Unit] =
    ScriptT[F, Any, Nothing, Unit](_ ⇒ F.pure((List(logLine), Right(()))))

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
    sa.logFailure(f)

  def logError[D, E, A](sa: ScriptT[F, D, E, A])(f: Throwable ⇒ LogLine)(implicit M: MonadError[F, Throwable]): ScriptT[F, D, E, A] =
    sa.logError(f)
}
