package purity.http4s

trait JsonClientContainer[F[+_]] {

  def jsonClient: JsonClient[F]
}
