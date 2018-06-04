package purity.store

import cats.implicits._
import cats.{Eq, Monad, Show}
import purity.implicits._
import purity.verification.PropositionalContext
import purity.verification.Truth.falsum

trait KVStoreTheory[F[_], K, V] extends PropositionalContext[F] {

  def store: KVStore[F, K, V]

  def indexIsContained
    (implicit
     eqK   : Eq[K],
     eqV   : Eq[V],
     showK : Show[K],
     showV : Show[V],
     F     : Monad[F]
    ): Proposition[F[Indexed[K, V]]] =
      indexF =>
        for {
          index <- indexF
          optRead <- store.read(index.key)
        } yield optRead match {
          case Some(read) => read =:= index
          case None => falsum(s"$index is not contained")
        }

  def indexIsForValue
    (value: V)
    (implicit
     eqK   : Eq[K],
     eqV   : Eq[V],
     showK : Show[K],
     showV : Show[V],
     F     : Monad[F]
    ): Proposition[F[Indexed[K, V]]] =
      _.map { _.value =:= value }
}
