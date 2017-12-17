package purity.logging

import cats.Applicative
import cats.implicits._
import cats.kernel.Monoid
import purity.logging.LogLevel._
import purity.logging.LogLine._

trait Logger[F[+_]] {

  protected val level: LogLevel

  protected def logEffect(line: LogLine): F[Unit]

  def log(line: LogLine)(implicit F: Applicative[F]): F[Unit] =
    if (line.level >= level) logEffect(line) else F.pure(Unit)

  def debug(message: String)(implicit F: Applicative[F]): F[Unit] = log(Debug(message, None))

  def debug(e: Throwable)(implicit F: Applicative[F]): F[Unit] = log(Debug(e.getMessage, Some(e)))

  def debug(message: String, e: Throwable)(implicit F: Applicative[F]): F[Unit] = log(Debug(message, Some(e)))

  def error(message: String)(implicit F: Applicative[F]): F[Unit] = log(Error(message, None))

  def error(e: Throwable)(implicit F: Applicative[F]): F[Unit] = log(Error(e.getMessage, Some(e)))

  def error(message: String, e: Throwable)(implicit F: Applicative[F]): F[Unit] = log(Error(message, Some(e)))

  def fatal(message: String)(implicit F: Applicative[F]): F[Unit] = log(Fatal(message, None))

  def fatal(e: Throwable)(implicit F: Applicative[F]): F[Unit] = log(Fatal(e.getMessage, Some(e)))

  def fatal(message: String, e: Throwable)(implicit F: Applicative[F]): F[Unit] = log(Fatal(message, Some(e)))

  def info(message: String)(implicit F: Applicative[F]): F[Unit] = log(Info(message, None))

  def info(e: Throwable)(implicit F: Applicative[F]): F[Unit] = log(Info(e.getMessage, Some(e)))

  def info(message: String, e: Throwable)(implicit F: Applicative[F]): F[Unit] = log(Info(message, Some(e)))

  def trace(message: String)(implicit F: Applicative[F]): F[Unit] = log(Trace(message, None))

  def trace(e: Throwable)(implicit F: Applicative[F]): F[Unit] = log(Trace(e.getMessage, Some(e)))

  def trace(message: String, e: Throwable)(implicit F: Applicative[F]): F[Unit] = log(Trace(message, Some(e)))

  def warn(message: String)(implicit F: Applicative[F]): F[Unit] = log(Warn(message, None))

  def warn(e: Throwable)(implicit F: Applicative[F]): F[Unit] = log(Warn(e.getMessage, Some(e)))

  def warn(message: String, e: Throwable)(implicit F: Applicative[F]): F[Unit] = log(Warn(message, Some(e)))

  def andThen(that: Logger[F])(implicit ev: Applicative[F]): Logger[F] =
    Logger[F]({
      line => log(line) *> that.log(line)
    }, if (level >= that.level) level else that.level)

  def compose(that: Logger[F])(implicit ev: Applicative[F]): Logger[F] =
    that.andThen(this)
}

object Logger {

  def apply[F[+_]](f: LogLine => F[Unit], l: LogLevel): Logger[F] =
    new Logger[F] {
      override val level: LogLevel = l
      override def logEffect(line: LogLine): F[Unit] = f(line)
    }

  def VoidLogs[F[+_]](implicit F: Applicative[F]): Logger[F] = Logger[F](_ => F.pure(()), OffLevel)

  implicit def monoidInstanceForLoggerFunctions[F[+_]](implicit F: Applicative[F]): Monoid[Logger[F]] =
    new Monoid[Logger[F]] {
      override def empty: Logger[F] = VoidLogs[F]
      override def combine(x: Logger[F], y: Logger[F]): Logger[F] = x andThen y
    }
}