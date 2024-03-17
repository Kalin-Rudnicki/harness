package harness.zio.error

import harness.core.*

sealed trait JarResourceError extends Throwable {

  override final def getMessage: String = this match {
    case JarResourceError.PathDNE(path) =>
      s"No such jar resource at path '$path'"
    case JarResourceError.Generic(path, cause) =>
      s"Encountered generic error when accessing jar resource at path '$path': ${cause.safeGetMessage}"
  }

}
object JarResourceError {
  final case class PathDNE(path: String) extends JarResourceError
  final case class Generic(path: String, cause: Throwable) extends JarResourceError
}
