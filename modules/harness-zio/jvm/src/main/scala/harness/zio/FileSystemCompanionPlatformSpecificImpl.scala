package harness.zio

import java.nio.file.FileSystem as JavaFileSystem
import java.nio.file.FileSystems as JavaFileSystems
import zio.*

trait FileSystemCompanionPlatformSpecificImpl { self: FileSystemCompanionPlatformSpecific =>

  def wrapJavaFileSystem(javaFileSystem: => JavaFileSystem): TaskLayer[FileSystem] =
    ZLayer.fromZIO(ZIO.attempt(WrappedJavaFileSystem(javaFileSystem)))

  def defaultJavaFileSystem: TaskLayer[FileSystem] =
    wrapJavaFileSystem(JavaFileSystems.getDefault)

  override val liveLayer: TaskLayer[FileSystem] =
    defaultJavaFileSystem

}
