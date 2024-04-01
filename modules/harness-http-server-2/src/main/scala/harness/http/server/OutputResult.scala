package harness.http.server

import harness.endpoint.transfer.*
import harness.zio.*
import zio.*

final case class OutputResult(
    length: Long,
    writeOutput: java.io.OutputStream => Unit,
)
object OutputResult {

  def fromString(string: String): OutputResult = {
    val bytes = string.getBytes
    OutputResult(bytes.length, _.write(bytes))
  }
  
  def fromOutputStream(outputStream: OutputStream): RIO[Scope, OutputResult] =
    outputStream match {
      case OutputStream.Empty =>
        ZIO.succeed(OutputResult(0L, _ => ()))
      case OutputStream.Str(string) =>
        val bytes = string.getBytes
        ZIO.succeed(OutputResult(bytes.length, _.write(bytes)))
      case OutputStream.File(path) =>
        for {
          length <- path.size
          stream <- path.inputStream
        } yield OutputResult(length, stream.transferTo)
      case OutputStream.Raw(stream) =>
        for {
          bytes <- ZIO.attempt(stream.readAllBytes())
          // TODO (KR) : handle if there is too many bytes
        } yield OutputResult(bytes.length, _.write(bytes))
    }

}
