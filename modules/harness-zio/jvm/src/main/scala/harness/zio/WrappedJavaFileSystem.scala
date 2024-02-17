package harness.zio

import harness.zio.error.FSError
import java.nio.file.FileSystem as JavaFileSystem
import scala.jdk.CollectionConverters.*
import zio.*

final case class WrappedJavaFileSystem(javaFileSystem: JavaFileSystem) extends FileSystem {

  override def path(string: String): IO[FSError.UnableToResolvePath, Path] =
    ZIO.attempt(WrappedJavaPath(javaFileSystem.getPath(string))).mapError(FSError.UnableToResolvePath(string, _))

  override def homeDirectory: IO[FSError.UnableToGetHomeDirectory | FSError.UnableToResolvePath, Path] =
    ZIO.attempt(java.lang.System.getProperty("user.home")).mapError(FSError.UnableToGetHomeDirectory(_)).flatMap(path)

  override def roots: IO[FSError.UnableToGetRoots, Chunk[Path]] =
    ZIO.attempt(Chunk.fromJavaIterable(javaFileSystem.getRootDirectories).map(WrappedJavaPath(_))).mapError(FSError.UnableToGetRoots(_))

}
