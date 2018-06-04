package purity.store

import cats.{Eq, Monad, Show}
import purity.verification.Specs

case class KVStoreSpec[F[_], K, V](store: KVStore[F, K, V]) extends KVStoreTheory[F, K, V] with Specs[F] {

  def saveSpec
    (implicit
     eqK   : Eq[K],
     eqV   : Eq[V],
     showV : Show[V],
     showK : Show[K],
     F     : Monad[F]
    ): Spec1[V, F[Indexed[K, V]]] =
      (value: V) => indexIsContained && indexIsForValue(value)
}

