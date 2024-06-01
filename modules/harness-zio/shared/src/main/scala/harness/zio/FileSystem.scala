package harness.zio

import harness.zio.error.FSError
import zio.*

trait FileSystem {
  def path(string: String): IO[FSError.UnableToResolvePath, Path]
  def homeDirectory: IO[FSError.UnableToGetHomeDirectory | FSError.UnableToResolvePath, Path]
  def roots: IO[FSError.UnableToGetRoots, Chunk[Path]]
}
object FileSystem extends FileSystemCompanionPlatformSpecific with FileSystemCompanionPlatformSpecificImpl {

  def path(string: String): ZIO[FileSystem, FSError.UnableToResolvePath, Path] = ZIO.service[FileSystem].flatMap(_.path(string))
  def homeDirectory: ZIO[FileSystem, FSError.UnableToGetHomeDirectory | FSError.UnableToResolvePath, Path] = ZIO.service[FileSystem].flatMap(_.homeDirectory)
  def roots: ZIO[FileSystem, FSError.UnableToGetRoots, Chunk[Path]] = ZIO.service[FileSystem].flatMap(_.roots)

  val unimplementedLayer: ULayer[FileSystem] = ZLayer.succeed { FileSystem.Unimplemented }
  
  case object Unimplemented extends FileSystem {
    def path(string: String): IO[FSError.UnableToResolvePath, Path] = ZIO.dieMessage("??? : FileSystem.path")
    def homeDirectory: IO[FSError.UnableToGetHomeDirectory | FSError.UnableToResolvePath, Path] = ZIO.dieMessage("??? : FileSystem.homeDirectory")
    def roots: IO[FSError.UnableToGetRoots, Chunk[Path]] = ZIO.dieMessage("??? : FileSystem.roots")
  }

}
