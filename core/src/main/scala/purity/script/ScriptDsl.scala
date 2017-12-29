package purity.script

import cats.effect.Effect
import cats.{Applicative, Functor, MonadError}
import purity.logging.LogLine._
import purity.logging.{LogLine, LoggerContainer}

trait ScriptDsl[F[+_]] {

  type Script[-Dependencies, +Failures, +Value] = ScriptT[F, Dependencies, Failures, Value]

  type Independent[+E, +A] = ScriptT[F, Any, E, A]

  type CantFail[-D, +A] = ScriptT[F, D, Nothing, A]

  type Value[+A] = ScriptT[F, Any, Nothing, A]

  type Failed[+E] = ScriptT[F, Any, E, Nothing]

  type SideEffect[-D] = ScriptT[F, D, Nothing, Unit]

  type Dependencies[D] = ScriptT[F, D, Nothing, D]

  type NoDependencies = Any

  def build[D, E, A](value: Either[E, A])(implicit F: Applicative[F]): ScriptT[F, D, E, A] =
    ScriptT[F, D, E, A](_ => F.pure(value))

  def pure[A](a: A)(implicit F: Applicative[F]): Value[A] =
    this.build(Right(a))

  def raiseFailure[E](e: E)(implicit F: Applicative[F]): Failed[E] =
    this.build(Left(e))

  def raiseError(e: Throwable)(implicit F: MonadError[F, Throwable]): SideEffect[NoDependencies] =
    ScriptT(_ => F.raiseError(e))

  def fail[E](e: E)(implicit F: Applicative[F]): Failed[E] =
    this.raiseFailure(e)

  def find[E, A](opt: Option[A])(e: => E)(implicit F: Applicative[F]): Independent[E, A] =
    opt.fold[ScriptT[F, Any, E, A]](fail(e))(pure(_))

  def find[E, A](opt: Either[E, A])(implicit F: Applicative[F]): Independent[E, A] =
    opt.fold[ScriptT[F, Any, E, A]](fail(_), pure(_))

  def unit(implicit F: Applicative[F]): SideEffect[NoDependencies] =
    this.pure(())

  def start(implicit F: Applicative[F]): SideEffect[NoDependencies] =
    this.unit

  def ok[E](implicit F: Applicative[F]): Independent[E, Unit] =
    this.unit

  def liftF[A](sa: F[A])(implicit F: Functor[F]): Value[A] =
    ScriptT[F, Any, Nothing, A](_ => F.map(sa)(a => Right(a)))

  def liftFE[E, A](fae: F[Either[E, A]])(implicit F: Functor[F]): Independent[E, A] =
    ScriptT[F, Any, E, A](_ => F.map(fae)(ea => ea))

  def script[A](sa: F[A])(implicit F: Functor[F]): Value[A] =
    this.liftF(sa)

  def scriptE[E, A](fae: F[Either[E, A]])(implicit F: Functor[F]): Independent[E, A] =
    this.liftFE(fae)

  def dependencies[D](implicit F: Applicative[F]): Dependencies[D] =
    ScriptT[F, D, Nothing, D](d => F.pure(Right(d)))

  object log {

    def logline(line: LogLine)(implicit F: Effect[F]): SideEffect[LoggerContainer[F]] =
      dependencies[LoggerContainer[F]].flatMap(l => script(l.logger.log(line)))

    def debug(message: String)(implicit F: Effect[F], fl: sourcecode.File, cl: sourcecode.Line): SideEffect[LoggerContainer[F]] =
      logline(Debug(message)(fl, cl))

    def debug(e: Throwable)(implicit F: Effect[F], fl: sourcecode.File, cl: sourcecode.Line): SideEffect[LoggerContainer[F]] =
      logline(Debug(e.getMessage, Some(e))(fl, cl))

    def debug(message: String, e: Throwable)(implicit F: Effect[F], fl: sourcecode.File, cl: sourcecode.Line): SideEffect[LoggerContainer[F]] =
      logline(Debug(message, Some(e))(fl, cl))

    def error(message: String)(implicit F: Effect[F], fl: sourcecode.File, cl: sourcecode.Line): SideEffect[LoggerContainer[F]] =
      logline(Error(message, None)(fl, cl))

    def error(e: Throwable)(implicit F: Effect[F], fl: sourcecode.File, cl: sourcecode.Line): SideEffect[LoggerContainer[F]] =
      logline(Error(e.getMessage, Some(e))(fl, cl))

    def error(message: String, e: Throwable)(implicit F: Effect[F], fl: sourcecode.File, cl: sourcecode.Line): SideEffect[LoggerContainer[F]] =
      logline(Error(message, Some(e))(fl, cl))

    def fatal(message: String)(implicit F: Effect[F], fl: sourcecode.File, cl: sourcecode.Line): SideEffect[LoggerContainer[F]] =
      logline(Fatal(message, None)(fl, cl))

    def fatal(e: Throwable)(implicit F: Effect[F], fl: sourcecode.File, cl: sourcecode.Line): SideEffect[LoggerContainer[F]] =
      logline(Fatal(e.getMessage, Some(e))(fl, cl))

    def fatal(message: String, e: Throwable)(implicit F: Effect[F], fl: sourcecode.File, cl: sourcecode.Line): SideEffect[LoggerContainer[F]] =
      logline(Fatal(message, Some(e))(fl, cl))

    def info(message: String)(implicit F: Effect[F], fl: sourcecode.File, cl: sourcecode.Line): SideEffect[LoggerContainer[F]] =
      logline(Info(message)(fl, cl))

    def info(e: Throwable)(implicit F: Effect[F], fl: sourcecode.File, cl: sourcecode.Line): SideEffect[LoggerContainer[F]] =
      logline(Info(e.getMessage, Some(e))(fl, cl))

    def info(message: String, e: Throwable)(implicit F: Effect[F], fl: sourcecode.File, cl: sourcecode.Line): SideEffect[LoggerContainer[F]] =
      logline(Info(message, Some(e))(fl, cl))

    def trace(message: String)(implicit F: Effect[F], fl: sourcecode.File, cl: sourcecode.Line): SideEffect[LoggerContainer[F]] =
      logline(Trace(message)(fl, cl))

    def trace(e: Throwable)(implicit F: Effect[F], fl: sourcecode.File, cl: sourcecode.Line): SideEffect[LoggerContainer[F]] =
      logline(Trace(e.getMessage, Some(e))(fl, cl))

    def trace(message: String, e: Throwable)(implicit F: Effect[F], fl: sourcecode.File, cl: sourcecode.Line): SideEffect[LoggerContainer[F]] =
      logline(Trace(message, Some(e))(fl, cl))

    def warn(message: String)(implicit F: Effect[F], fl: sourcecode.File, cl: sourcecode.Line): SideEffect[LoggerContainer[F]] =
      logline(Warn(message)(fl, cl))

    def warn(e: Throwable)(implicit F: Effect[F], fl: sourcecode.File, cl: sourcecode.Line): SideEffect[LoggerContainer[F]] =
      logline(Warn(e.getMessage, Some(e))(fl, cl))

    def warn(message: String, e: Throwable)(implicit F: Effect[F], fl: sourcecode.File, cl: sourcecode.Line): SideEffect[LoggerContainer[F]] =
      logline(Warn(message, Some(e))(fl, cl))
  }
}
