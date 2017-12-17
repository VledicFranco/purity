package purity

import cats.{Applicative, Contravariant, MonadError}
import cats.data.NonEmptyList
import purity.script.{ScriptDsl, ScriptT}
import purity.Truth.{False, True}

/**
 * A data type for lazy consistency checks on a data structure.
 *
 * A [[Proposition]] wraps a function that checks a proposition of a logical statement over a type. Combinators like leftMap,
 * contramap, &&, ||, and not, can be used to easily create more complex checks.
 *
 * {{{
 * val a = Proposition[String, Int](x => if(x > 10) False("age should be less than 10") else True)
 * //a: purity.Proposition[String,Int] = Proposition
 *
 * a.check(21)
 * //res0: purity.Truth[String] = False(NonEmptyList(age should be less than 10))
 *
 * a.check(9)
 * //res1: purity.Truth[String] = True
 *
 * val b = Proposition[String, String](x => if(x.length > 2) False("name code should be less than 3") else True)
 * //b: purity.Proposition[String,String] = Proposition
 *
 * val c = a.contramap[User](_.age) && b.contramap[User](_.name)
 * //c: purity.Proposition[String,User] = Proposition
 *
 * c.check(User("FAF", 21))
 * //res3: purity.Truth[String] = False(NonEmptyList(age should be less than 10, name code should be less than 3))
 * }}}
 *
 * @param check function. Should check for proposition of a logical statement over type A
 * @tparam E type of the failure in case of an False result.
 * @tparam A type to be checked for consistency.
 */
case class Proposition[+E, -A](check: A => Truth[E]) {

  def contramap[B](f: B => A): Proposition[E, B] =
    Proposition(f andThen check)

  def leftMap[E2](f: E => E2): Proposition[E2, A] =
    Proposition[E2, A](check(_).map(f))

  def not[EE >: E](e: EE): Proposition[EE, A] =
    Proposition[EE, A](check(_).not(e))

  def &&[EE >: E, AA <: A](g: Proposition[EE, AA]): Proposition[EE, AA] =
    Proposition[EE, AA](a => check(a) && g.check(a))

  def ||[EE >: E, AA <: A](g: Proposition[EE, AA]): Proposition[EE, AA] =
    Proposition[EE, AA](a => check(a) || g.check(a))

  def optional: Proposition[E, Option[A]] =
    Proposition {
      case Some(a) => check(a)
      case None => True
    }

  def required[EE >: E](e: EE): Proposition[EE, Option[A]] =
    Proposition {
      case Some(a) => check(a)
      case None => False(e)
    }

  def script[F[+_], E2](dsl: ScriptDsl[F])(a: A)(implicit ev: Applicative[F]): dsl.Independent[NonEmptyList[E], Unit] =
    check(a) match {
      case True     => dsl.ok
      case False(e) => dsl.fail(e)
    }

  override def toString: String = "Proposition"
}

object Proposition extends PropositionFunctions with PropositionInstances

private[purity] trait PropositionFunctions {

  def thatIsTrue[E, A]: Proposition[E, A] = Proposition(_ => True)

  def thatIsFalse[E, A](e: E): Proposition[E, A] = Proposition(_ => Truth.isFalse(e))
}

private[purity] trait PropositionInstances {

  implicit def stdContravariant[E]: Contravariant[Proposition[E, ?]] =
    contravariantAnd[E]

  def contravariantAnd[E]: Contravariant[Proposition[E, ?]] =
    new Contravariant[Proposition[E, ?]] {
      override def contramap[A, B](fa: Proposition[E, A])(f: (B) => A): Proposition[E, B] =
        fa.contramap(f)
    }

  def contravariantOr[E]: Contravariant[Proposition[E, ?]] =
    new Contravariant[Proposition[E, ?]] {
      override def contramap[A, B](fa: Proposition[E, A])(f: (B) => A): Proposition[E, B] =
        fa.contramap(f)
    }
}