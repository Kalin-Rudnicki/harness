package harness.zio

import harness.zio.error.FSError
import zio.*

trait FileSystem {
  def path(string: String): IO[FSError.UnableToResolvePath, Path]
  def homeDirectory: IO[FSError.UnableToGetHomeDirectory | FSError.UnableToResolvePath, Path]
  def roots: IO[FSError.UnableToGetRoots, Chunk[Path]]
}
object FileSystem extends FileSystemCompanionPlatformSpecific with FileSystemCompanionPlatformSpecificImpl {

  // =====|  |=====

  private[zio] val fileSystemRef: FiberRef[FileSystem] =
    Unsafe.unsafely {
      FiberRef.unsafe.make[FileSystem](
        defaultFS,
        identity,
        (_, child) => child,
      )
    }

  // =====| API |=====

  // --- Modify ---

  def withFileSystem(f: FileSystem): FiberRefModification =
    FileSystem.fileSystemRef.modification.set(f)
  def withFileSystem(f: FileSystem => FileSystem): FiberRefModification =
    FileSystem.fileSystemRef.modification.update(f)

  // --- Execute ---

  def path(string: String): IO[FSError.UnableToResolvePath, Path] = FileSystem.fileSystemRef.getWith(_.path(string))
  def homeDirectory: IO[FSError.UnableToGetHomeDirectory | FSError.UnableToResolvePath, Path] = FileSystem.fileSystemRef.getWith(_.homeDirectory)
  def roots: IO[FSError.UnableToGetRoots, Chunk[Path]] = FileSystem.fileSystemRef.getWith(_.roots)

}

case object UnimplementedFileSystem extends FileSystem {
  def path(string: String): IO[FSError.UnableToResolvePath, Path] = ZIO.dieMessage("??? : FileSystem.path")
  def homeDirectory: IO[FSError.UnableToGetHomeDirectory | FSError.UnableToResolvePath, Path] = ZIO.dieMessage("??? : FileSystem.homeDirectory")
  def roots: IO[FSError.UnableToGetRoots, Chunk[Path]] = ZIO.dieMessage("??? : FileSystem.roots")
}
