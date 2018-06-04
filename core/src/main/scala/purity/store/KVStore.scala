package purity.store

trait KVStore[F[_], K, V] {

  def save(value: V): F[Indexed[K, V]]

  def read(key: K): F[Option[Indexed[K, V]]]

  def update(key: K)(f: V => V): F[Indexed[K, V]]

  def delete(key: K): F[Unit]

  def put(value: V): F[Indexed[K, V]]
}
