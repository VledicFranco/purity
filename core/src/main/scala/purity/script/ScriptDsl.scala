package purity.script

import cats.{ Applicative, Functor }
import purity.logging.LogLine._
import purity.logging.LogLine

trait ScriptDsl[F[+_]] {

  type Script[-D, +E, +A] = ScriptT[F, D, E, A]

  type Independent[+E, +A] = ScriptT[F, Any, E, A]

  type CantFail[-D, +A] = ScriptT[F, D, Nothing, A]

  type Value[+A] = ScriptT[F, Any, Nothing, A]

  type Failed[+E] = ScriptT[F, Any, E, Nothing]

  type SideEffect = ScriptT[F, Any, Nothing, Unit]

  type Dependencies[D] = ScriptT[F, D, Nothing, D]

  def build[D, E, A](value: Either[E, A], logs: List[LogLine])(implicit F: Applicative[F]): ScriptT[F, D, E, A] =
    ScriptT[F, D, E, A](_ ⇒ F.pure((logs, value)))

  def buildNoLogs[D, E, A](value: Either[E, A])(implicit F: Applicative[F]): ScriptT[F, D, E, A] =
    this.build[D, E, A](value, Nil)

  def pure[A](a: A)(implicit F: Applicative[F]): Value[A] =
    this.buildNoLogs(Right(a))

  def raiseError[E](e: E)(implicit F: Applicative[F]): Failed[E] =
    this.buildNoLogs(Left(e))

  def raiseError[E](e: E, logLine: LogLine)(implicit F: Applicative[F]): Failed[E] =
    this.build(Left(e), List(logLine))

  def fail[E](e: E)(implicit F: Applicative[F]): Failed[E] =
    this.raiseError(e)

  def fail[E](e: E, logLine: LogLine)(implicit F: Applicative[F]): Failed[E] =
    this.raiseError(e, logLine)

  def find[E, A](e: => E)(opt: Option[A])(implicit F: Applicative[F]): Independent[E, A] =
    opt.fold[ScriptT[F, Any, E, A]](fail(e))(pure(_))

  def find[E, A](opt: Either[E, A])(implicit F: Applicative[F]): Independent[E, A] =
    opt.fold[ScriptT[F, Any, E, A]](fail(_), pure(_))

  def unit(implicit F: Applicative[F]): SideEffect =
    this.pure(())

  def start(implicit F: Applicative[F]): SideEffect =
    this.unit

  def ok[E](implicit F: Applicative[F]): Independent[E, Unit] =
    this.unit

  def liftF[A](sa: F[A])(implicit F: Functor[F]): Value[A] =
    ScriptT[F, Any, Nothing, A](_ ⇒ F.map(sa)(a ⇒ (Nil, Right(a))))

  def liftFE[E, A](fae: F[Either[E, A]])(implicit F: Functor[F]): Independent[E, A] =
    ScriptT[F, Any, E, A](_ ⇒ F.map(fae)(ea ⇒ (Nil, ea)))

  def script[A](sa: F[A])(implicit F: Functor[F]): Value[A] =
    this.liftF(sa)

  def scriptE[E, A](fae: F[Either[E, A]])(implicit F: Functor[F]): Independent[E, A] =
    this.liftFE(fae)

  def dependencies[D](implicit F: Applicative[F]): Dependencies[D] =
    ScriptT[F, D, Nothing, D](d ⇒ F.pure((Nil, Right(d))))

  object log {

    def liftLog(logLine: LogLine)(implicit F: Applicative[F]): SideEffect =
      ScriptT[F, Any, Nothing, Unit](_ ⇒ F.pure((List(logLine), Right(()))))

    def debug(message: String)(implicit F: Applicative[F]): SideEffect =
      liftLog(Debug(message))

    def debug(e: Throwable)(implicit F: Applicative[F]): SideEffect =
      liftLog(Debug(e.getMessage, Some(e)))

    def debug(message: String, e: Throwable)(implicit F: Applicative[F]): SideEffect =
      liftLog(Debug(message, Some(e)))

    def error(message: String)(implicit F: Applicative[F]): SideEffect =
      liftLog(Error(message, None))

    def error(e: Throwable)(implicit F: Applicative[F]): SideEffect =
      liftLog(Error(e.getMessage, Some(e)))

    def error(message: String, e: Throwable)(implicit F: Applicative[F]): SideEffect =
      liftLog(Error(message, Some(e)))

    def fatal(message: String)(implicit F: Applicative[F]): SideEffect =
      liftLog(Fatal(message, None))

    def fatal(e: Throwable)(implicit F: Applicative[F]): SideEffect =
      liftLog(Fatal(e.getMessage, Some(e)))

    def fatal(message: String, e: Throwable)(implicit F: Applicative[F]): SideEffect =
      liftLog(Fatal(message, Some(e)))

    def info(message: String)(implicit F: Applicative[F]): SideEffect =
      liftLog(Info(message))

    def info(e: Throwable)(implicit F: Applicative[F]): SideEffect =
      liftLog(Info(e.getMessage, Some(e)))

    def info(message: String, e: Throwable)(implicit F: Applicative[F]): SideEffect =
      liftLog(Info(message, Some(e)))

    def trace(message: String)(implicit F: Applicative[F]): SideEffect =
      liftLog(Trace(message))

    def trace(e: Throwable)(implicit F: Applicative[F]): SideEffect =
      liftLog(Trace(e.getMessage, Some(e)))

    def trace(message: String, e: Throwable)(implicit F: Applicative[F]): SideEffect =
      liftLog(Trace(message, Some(e)))

    def warn(message: String)(implicit F: Applicative[F]): SideEffect =
      liftLog(Warn(message))

    def warn(e: Throwable)(implicit F: Applicative[F]): SideEffect =
      liftLog(Warn(e.getMessage, Some(e)))

    def warn(message: String, e: Throwable)(implicit F: Applicative[F]): SideEffect =
      liftLog(Warn(message, Some(e)))
  }
}
