package harness.sql.error

import harness.core.*

sealed trait ConnectionError extends Throwable {

  override final def getMessage: String = this match {
    case ConnectionError.Generic(cause) =>
      s"Generic connection error (${cause.getClass.getName}): ${cause.safeGetMessage}"
  }

}
object ConnectionError {

  final case class Generic(cause: Throwable) extends ConnectionError

  // TODO (KR) : handle more specific cases
  def apply(cause: Throwable): ConnectionError =
    ConnectionError.Generic(cause)

}
