package harness.zio.error

sealed trait JarResourceError extends Throwable
object JarResourceError {
  final case class PathDNE(path: String) extends JarResourceError
  final case class Generic(path: String, cause: Throwable) extends JarResourceError
}
