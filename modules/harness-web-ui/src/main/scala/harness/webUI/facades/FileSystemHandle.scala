package harness.webUI.facades

import harness.webUI.error.UIError
import org.scalajs.dom.File
import scala.scalajs.js
import scala.scalajs.js.annotation.*
import zio.*
import zio.stream.*

@js.native
@JSGlobal
class FileSystemHandle extends js.Any {
  def kind: String = js.native
  def name: String = js.native
}

@js.native
@JSGlobal
class FileSystemFileHandle extends FileSystemHandle {
  def getFile(): js.Promise[File] = js.native
}

@js.native
@JSGlobal
class FileSystemDirectoryHandle extends FileSystemHandle {
  def values(): FileSystemDirectoryIterator = js.native
}

def filesForChunk(chunk: Chunk[FileSystemHandle]): Stream[UIError.Failure, (Option[String], File)] =
  ZStream.concatAll(chunk.map(_.files))

extension (fsh: FileSystemHandle) {
  def files: Stream[UIError.Failure, (Option[String], File)] = {
    def loop(fsh: FileSystemHandle, rPath: List[String]): Stream[UIError.Failure, (Option[String], File)] =
      fsh match {
        case handle: FileSystemDirectoryHandle =>
          ZStream.fromZIO(handle.children).flatMap { children =>
            val newRPath = handle.name :: rPath
            ZStream.concatAll(children.map(loop(_, newRPath)))
          }
        case handle: FileSystemFileHandle => ZStream.fromZIO(handle.file.map(file => (Option.when(rPath.nonEmpty)(rPath.reverse.mkString("/")), file)))
        case h                            => ZStream.fail(UIError.Failure.internalDefect(s"shouldn't be possible... : $h"))
      }

    loop(fsh, Nil)
  }
}
extension (fsfh: FileSystemFileHandle) {
  def file: IO[UIError.Failure, File] = ZIO.fromPromiseJS(fsfh.getFile()).mapError(UIError.Failure.internalDefect(_))
}

extension (fsdh: FileSystemDirectoryHandle) {
  def children: IO[UIError.Failure, Chunk[FileSystemHandle]] = fsdh.values().toZIOChunk
}
