package purity.store

import cats.{Eq, Show}

case class Indexed[K, V](key: K, value: V)

object Indexed {

  implicit def EqIndexedKV[K, V](implicit eqK: Eq[K], eqV: Eq[V]): Eq[Indexed[K, V]] =
    (x: Indexed[K, V], y: Indexed[K, V]) => eqK.eqv(x.key, y.key) && eqV.eqv(x.value, y.value)

  implicit def ShowIndexedKV[K, V](implicit eqK: Show[K], eqV: Show[V]): Show[Indexed[K, V]] =
    (ident: Indexed[K, V]) => s"(${eqK.show(ident.key)}: ${eqV.show(ident.value)})"
}

