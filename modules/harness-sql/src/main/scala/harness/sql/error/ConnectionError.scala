package harness.sql.error

sealed trait ConnectionError extends Throwable
object ConnectionError {

  final case class Generic(cause: Throwable) extends ConnectionError

  // TODO (KR) : handle more specific cases
  def apply(cause: Throwable): ConnectionError =
    ConnectionError.Generic(cause)

}
