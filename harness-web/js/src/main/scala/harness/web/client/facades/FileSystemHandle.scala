package harness.web.client.facades

import harness.core.*
import harness.zio.*
import org.scalajs.dom.File
import zio.*
import zio.stream.*

import scala.scalajs.js
import scala.scalajs.js.annotation.*

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

def filesForChunk(chunk: Chunk[FileSystemHandle]): Stream[HError, (Option[String], File)] =
  ZStream.concatAll(chunk.map(_.files))

extension (fsh: FileSystemHandle) {
  def files: Stream[HError, (Option[String], File)] = {
    def loop(fsh: FileSystemHandle, rPath: List[String]): Stream[HError, (Option[String], File)] =
      fsh match {
        case handle: FileSystemDirectoryHandle =>
          ZStream.fromZIO(handle.children).flatMap { children =>
            val newRPath = handle.name :: rPath
            ZStream.concatAll(children.map(loop(_, newRPath)))
          }
        case handle: FileSystemFileHandle => ZStream.fromZIO(handle.file.map(file => (Option.when(rPath.nonEmpty)(rPath.reverse.mkString("/")), file)))
        case h                            => ZStream.fail(HError.InternalDefect(s"shouldn't be possible... : $h"))
      }

    loop(fsh, Nil)
  }
}
extension (fsfh: FileSystemFileHandle) {
  def file: HTask[File] = ZIO.fromPromiseJS(fsfh.getFile()).mapError(HError.SystemFailure("failed to get file", _))
}

extension (fsdh: FileSystemDirectoryHandle) {
  def children: HTask[Chunk[FileSystemHandle]] = fsdh.values().toChunk
}
