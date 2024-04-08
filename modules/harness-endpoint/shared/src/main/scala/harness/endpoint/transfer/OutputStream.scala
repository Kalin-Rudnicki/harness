package harness.endpoint.transfer

import harness.zio.Path

sealed trait OutputStream
object OutputStream {
  case object Empty extends OutputStream

  sealed trait NonEmpty extends OutputStream
  final case class Str(string: String) extends OutputStream.NonEmpty
  final case class File(path: Path) extends OutputStream.NonEmpty
  final case class ForwardRaw(stream: java.io.InputStream) extends OutputStream.NonEmpty
}
