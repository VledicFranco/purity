package purity

import cats.Monad
import cats.data.Validated.{ Invalid, Valid }
import cats.data.ValidatedNel
import purity.Truth.{ True, False }

case class ConsistentParse[E, A, B](p: Proposition[E, B], parsing: A ⇒ ValidatedNel[E, B]) {

  def run(a: A): ValidatedNel[E, B] =
    parsing(a) andThen { b ⇒
      p.check(b) match {
        case True     ⇒ Valid(b)
        case False(e) ⇒ Invalid(e)
      }
    }

  def map[C](f: B ⇒ C): ConsistentParse[E, A, C] =
    ConsistentParse.sound(run(_).map(f))

  def flatMap[C](f: B ⇒ ConsistentParse[E, A, C]): ConsistentParse[E, A, C] =
    ConsistentParse.sound { a ⇒
      run(a) andThen (f(_).run(a))
    }

  def leftMap[E2](f: E ⇒ E2): ConsistentParse[E2, A, B] =
    ConsistentParse.parse(p.leftMap(f))(parsing(_).leftMap(_.map(f)))

  def contramap[C](f: C ⇒ A): ConsistentParse[E, C, B] =
    ConsistentParse.parse(p)(f andThen parsing)

  def dimap[C, D](f: C ⇒ A)(g: B ⇒ D): ConsistentParse[E, C, D] =
    ConsistentParse.sound[E, C, D](f andThen run andThen (_.map(g)))

  def not(e: E): ConsistentParse[E, A, B] =
    ConsistentParse.parse(p.not(e))(parsing)

  def &&(q: Proposition[E, B]): ConsistentParse[E, A, B] =
    ConsistentParse.parse(p && q)(parsing)

  def ||(q: Proposition[E, B]): ConsistentParse[E, A, B] =
    ConsistentParse.parse(p || q)(parsing)
}

object ConsistentParse extends ConsistentFunctions with ConsistentInstances

private[purity] trait ConsistentFunctions {

  def pure[E, A, B](b: B): ConsistentParse[E, A, B] =
    ConsistentParse.sound[E, A, B](_ ⇒ Valid(b))

  def ask[E, A]: ConsistentParse[E, A, A] =
    ConsistentParse.parse[E, A, A](Proposition.thatIsTrue)(Valid(_))

  def parse[E, A, B](Proposition: Proposition[E, B])(f: A ⇒ ValidatedNel[E, B]): ConsistentParse[E, A, B] =
    ConsistentParse(Proposition, f)

  def sound[E, A, B](f: A ⇒ ValidatedNel[E, B]): ConsistentParse[E, A, B] =
    ConsistentParse.parse[E, A, B](Proposition.thatIsTrue)(f)

  def unsound[E, A](e: E): ConsistentParse[E, A, A] =
    ConsistentParse.parse[E, A, A](Proposition.thatIsFalse(e))(Valid(_))
}

private[purity] trait ConsistentInstances {

  implicit def stdMonadErrorForConsistent[E, A]: Monad[ConsistentParse[E, A, ?]] =
    new Monad[ConsistentParse[E, A, ?]] {
      override def flatMap[B, C](fa: ConsistentParse[E, A, B])(f: (B) ⇒ ConsistentParse[E, A, C]): ConsistentParse[E, A, C] =
        fa.flatMap[C](f)

      override def tailRecM[B, C](b: B)(f: (B) ⇒ ConsistentParse[E, A, Either[B, C]]): ConsistentParse[E, A, C] =
        f(b).flatMap {
          case Left(b1) ⇒ tailRecM(b1)(f)
          case Right(c) ⇒ ConsistentParse.pure[E, A, C](c)
        }

      override def pure[B](x: B): ConsistentParse[E, A, B] =
        ConsistentParse.pure(x)
    }
}
