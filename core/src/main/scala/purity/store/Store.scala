package purity.store

import cats.{Eq, Monad, Show}
import cats.implicits._
import purity.implicits._
import purity.verification._
import purity.verification.Propositions
import purity.verification.Truth.falsum

case class Ident[K, V](key: K, value: V)

object Ident {

  implicit def eqInstanceForIdent[K, V](implicit eqK: Eq[K], eqV: Eq[V]): Eq[Ident[K, V]] =
    (x: Ident[K, V], y: Ident[K, V]) => eqK.eqv(x.key, y.key) && eqV.eqv(x.value, y.value)

  implicit def showInstanceForIdent[K, V](implicit eqK: Show[K], eqV: Show[V]): Show[Ident[K, V]] =
    (ident: Ident[K, V]) => s"(${eqK.show(ident.key)}: ${eqV.show(ident.value)})"
}

trait Store[F[_], K, V] {

  def save(value: V): F[Ident[K, V]]

  def read(key: K): F[Option[Ident[K, V]]]

  def update(key: K)(f: V => V): F[Ident[K, V]]

  def delete(key: K): F[Unit]

  def put(value: V): F[Ident[K, V]]
}

trait StoreSpec[F[_], K, V] extends StorePropositions[F, K, V] with Specs[F] {

  def saveSpec(implicit eqK: Eq[K], eqV: Eq[V], showV: Show[V], showK: Show[K], F: Monad[F]): Spec1[V, F[Ident[K, V]]] =
    (value: V) => identIsContained && identHasValue(value)
}

trait StorePropositions[F[_], K, V] extends Propositions[F] {

  def store: Store[F, K, V]

  def identIsContained(implicit eqK: Eq[K], eqV: Eq[V], showV: Show[V], showK: Show[K], F: Monad[F]): Proposition[F[Ident[K, V]]] =
    identM =>
      for {
        ident <- identM
        optRead <- store.read(ident.key)
      } yield optRead match {
        case Some(read) => read =:= ident
        case None => falsum(s"$ident is not contained")
      }

  def identHasValue(value: V)(implicit eq: Eq[V], showV: Show[V], F: Monad[F]): Proposition[F[Ident[K, V]]] =
    _.map { _.value =:= value }
}
