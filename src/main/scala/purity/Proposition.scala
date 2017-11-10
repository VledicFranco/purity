package purity

import purity.Truth.{ False, True }
import cats.{ Contravariant, MonadError }
import cats.data.NonEmptyList
import purity.script.{ ScriptDSL, ScriptT }

/**
 * A data type for lazy consistency checks on a data structure.
 *
 * A [[Proposition]] wraps a function that checks a proposition of a logical statement over a type. Combinators like leftMap,
 * contramap, &&, ||, and not, can be used to easily create more complex checks.
 *
 * {{{
 * scala> val a = Proposition[String, Int](x => if(x > 10) False("age should be less than 10") else True)
 * a: purity.Proposition[String,Int] = Proposition($$Lambda$7806/1294210606@142fe5e1)
 *
 * scala> a.check(21)
 * res0: purity.Proposition[String] = False(NonEmptyList(age should be less than 10))
 *
 * scala> a.check(9)
 * res1: purity.Proposition[String] = True
 *
 * scala> val b = Proposition[String, String](x => if(x.length > 2) False("name code should be less than 3") else True)
 * b: purity.Proposition[String,String] = Proposition($$Lambda$7807/2059704745@1b0408f0)
 *
 * scala> val c = a.contramap[User](_.age) && b.contramap[User](_.name)
 * c: purity.Proposition[String,User] = Proposition(purity.Proposition$$Lambda$7813/1772294329@5ea95f08)
 *
 * scala> c.check(User("FAF", 21))
 * res3: purity.Proposition[String] = False(NonEmptyList(age should be less than 10, name code should be less than 3))
 * }}}
 *
 * @param check function. Should check for proposition of a logical statement over type A
 * @tparam E type of the failure in case of an False result.
 * @tparam A type to be checked for consistency.
 */
case class Proposition[+E, -A](check: A ⇒ Truth[E]) {

  def contramap[B](f: B ⇒ A): Proposition[E, B] =
    Proposition(f andThen check)

  def leftMap[E2](f: E ⇒ E2): Proposition[E2, A] =
    Proposition[E2, A](check(_).map(f))

  def not[EE >: E](e: EE): Proposition[EE, A] =
    Proposition[EE, A](check(_).not(e))

  def &&[EE >: E, AA <: A](g: Proposition[EE, AA]): Proposition[EE, AA] =
    Proposition[EE, AA](a ⇒ check(a) && g.check(a))

  def ||[EE >: E, AA <: A](g: Proposition[EE, AA]): Proposition[EE, AA] =
    Proposition[EE, AA](a ⇒ check(a) || g.check(a))

  def script[F[+_], E2](dsl: ScriptDSL[F])(a: A)(implicit ev: MonadError[F, Throwable]): ScriptT[F, Any, NonEmptyList[E], Unit] =
    check(a) match {
      case True     ⇒ dsl.ok
      case False(e) ⇒ dsl.fail(e)
    }
}

object Proposition extends PropositionFunctions with PropositionInstances

private[purity] trait PropositionFunctions {

  def thatIsTrue[E, A]: Proposition[E, A] = Proposition(_ ⇒ True)

  def thatIsFalse[E, A](e: E): Proposition[E, A] = Proposition(_ ⇒ Truth.isFalse(e))
}

private[purity] trait PropositionInstances {

  implicit def stdContravariantCartesian[E]: Contravariant[Proposition[E, ?]] =
    contravariantCartesianAnd[E]

  def contravariantCartesianAnd[E]: Contravariant[Proposition[E, ?]] =
    new Contravariant[Proposition[E, ?]] {
      override def contramap[A, B](fa: Proposition[E, A])(f: (B) ⇒ A): Proposition[E, B] =
        fa.contramap(f)
    }

  def contravariantCartesianOr[E]: Contravariant[Proposition[E, ?]] =
    new Contravariant[Proposition[E, ?]] {
      override def contramap[A, B](fa: Proposition[E, A])(f: (B) ⇒ A): Proposition[E, B] =
        fa.contramap(f)
    }
}