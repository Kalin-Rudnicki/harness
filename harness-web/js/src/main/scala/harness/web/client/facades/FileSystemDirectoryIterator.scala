package harness.web.client.facades

import cats.syntax.option.*
import harness.core.*
import harness.zio.*
import zio.*
import zio.stream.*

import scala.scalajs.js
import scala.scalajs.js.annotation.*

@js.native
@JSGlobal
class FileSystemDirectoryIterator extends js.Any {
  def next(): js.Promise[js.Iterator.Entry[FileSystemHandle]] = js.native
}
object FileSystemDirectoryIterator {
  extension (fsdi: FileSystemDirectoryIterator) {
    def toChunk: HTask[Chunk[FileSystemHandle]] =
      ZStream
        .repeatZIOOption(
          ZIO
            .fromPromiseJS(fsdi.next())
            .foldZIO(
              t => ZIO.fail(HError.InternalDefect("error in FileSystemDirectoryIterator", t).some),
              entry =>
                if (entry.done) ZIO.fail(None)
                else ZIO.succeed(entry.value),
            ),
        )
        .runCollect
  }
}
