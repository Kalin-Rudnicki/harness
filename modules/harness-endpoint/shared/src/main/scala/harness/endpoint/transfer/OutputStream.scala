package harness.endpoint.transfer

import harness.zio.Path

sealed trait OutputStream
object OutputStream {
  case object Empty extends OutputStream
  final case class Str(string: String) extends OutputStream
  final case class File(path: Path) extends OutputStream
  final case class Raw(stream: java.io.InputStream) extends OutputStream
}
