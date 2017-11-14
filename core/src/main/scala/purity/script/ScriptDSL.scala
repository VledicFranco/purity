package purity.script

import cats.{ Applicative, Functor }
import purity.logging.LogLine._
import purity.logging.LogLine

trait ScriptDSL[F[+_]] {

  type Script[-D, +E, +A] = ScriptT[F, D, E, A]

  def pure[A](a: A)(implicit F: Applicative[F]): ScriptT[F, Any, Nothing, A] =
    ScriptT[F, Any, Nothing, A](_ ⇒ F.pure((Nil, Right(a))))

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

  def dependencies[D](implicit F: Applicative[F]): ScriptT[F, D, Nothing, D] =
    ScriptT[F, D, Nothing, D](d ⇒ F.pure((Nil, Right(d))))

  object log {

    def liftLog(logLine: LogLine)(implicit F: Applicative[F]): ScriptT[F, Any, Nothing, Unit] =
      ScriptT[F, Any, Nothing, Unit](_ ⇒ F.pure((List(logLine), Right(()))))

    def debug(message: String)(implicit F: Applicative[F]): ScriptT[F, Any, Nothing, Unit] =
      liftLog(Debug(message))

    def debug(e: Throwable)(implicit F: Applicative[F]): ScriptT[F, Any, Nothing, Unit] =
      liftLog(Debug(e.getMessage, Some(e)))

    def debug(message: String, e: Throwable)(implicit F: Applicative[F]): ScriptT[F, Any, Nothing, Unit] =
      liftLog(Debug(message, Some(e)))

    def error(message: String)(implicit F: Applicative[F]): ScriptT[F, Any, Nothing, Unit] =
      liftLog(Error(message, None))

    def error(e: Throwable)(implicit F: Applicative[F]): ScriptT[F, Any, Nothing, Unit] =
      liftLog(Error(e.getMessage, Some(e)))

    def error(message: String, e: Throwable)(implicit F: Applicative[F]): ScriptT[F, Any, Nothing, Unit] =
      liftLog(Error(message, Some(e)))

    def fatal(message: String)(implicit F: Applicative[F]): ScriptT[F, Any, Nothing, Unit] =
      liftLog(Fatal(message, None))

    def fatal(e: Throwable)(implicit F: Applicative[F]): ScriptT[F, Any, Nothing, Unit] =
      liftLog(Fatal(e.getMessage, Some(e)))

    def fatal(message: String, e: Throwable)(implicit F: Applicative[F]): ScriptT[F, Any, Nothing, Unit] =
      liftLog(Fatal(message, Some(e)))

    def info(message: String)(implicit F: Applicative[F]): ScriptT[F, Any, Nothing, Unit] =
      liftLog(Info(message))

    def info(e: Throwable)(implicit F: Applicative[F]): ScriptT[F, Any, Nothing, Unit] =
      liftLog(Info(e.getMessage, Some(e)))

    def info(message: String, e: Throwable)(implicit F: Applicative[F]): ScriptT[F, Any, Nothing, Unit] =
      liftLog(Info(message, Some(e)))

    def trace(message: String)(implicit F: Applicative[F]): ScriptT[F, Any, Nothing, Unit] =
      liftLog(Trace(message))

    def trace(e: Throwable)(implicit F: Applicative[F]): ScriptT[F, Any, Nothing, Unit] =
      liftLog(Trace(e.getMessage, Some(e)))

    def trace(message: String, e: Throwable)(implicit F: Applicative[F]): ScriptT[F, Any, Nothing, Unit] =
      liftLog(Trace(message, Some(e)))

    def warn(message: String)(implicit F: Applicative[F]): ScriptT[F, Any, Nothing, Unit] =
      liftLog(Warn(message))

    def warn(e: Throwable)(implicit F: Applicative[F]): ScriptT[F, Any, Nothing, Unit] =
      liftLog(Warn(e.getMessage, Some(e)))

    def warn(message: String, e: Throwable)(implicit F: Applicative[F]): ScriptT[F, Any, Nothing, Unit] =
      liftLog(Warn(message, Some(e)))
  }
}
