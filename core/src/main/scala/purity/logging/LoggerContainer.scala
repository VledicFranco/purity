package purity.logging

trait LoggerContainer[F[+_]] {

  def logger: Logger[F]
}
