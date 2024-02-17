package harness.webUI.facades

import cats.syntax.option.*
import harness.webUI.error.UIError
import scala.scalajs.js
import scala.scalajs.js.annotation.*
import zio.*
import zio.stream.*

@js.native
@JSGlobal
class FileSystemDirectoryIterator extends js.Any {
  def next(): js.Promise[js.Iterator.Entry[FileSystemHandle]] = js.native
}
object FileSystemDirectoryIterator {
  extension (fsdi: FileSystemDirectoryIterator) {
    def toZIOChunk: IO[UIError.Failure, Chunk[FileSystemHandle]] =
      ZStream
        .repeatZIOOption(
          ZIO
            .fromPromiseJS(fsdi.next())
            .foldZIO(
              t => ZIO.fail(UIError.Failure.internalDefect(t).some),
              entry =>
                if (entry.done) ZIO.fail(None)
                else ZIO.succeed(entry.value),
            ),
        )
        .runCollect
  }
}
