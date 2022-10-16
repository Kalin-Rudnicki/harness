package harness.zio

import harness.core.*
import java.nio.file.FileSystem as JavaFileSystem
import java.nio.file.FileSystems as JavaFileSystems
import zio.*

trait FileSystemCompanionPlatformSpecificImpl { self: FileSystemCompanionPlatformSpecific =>

  def wrapJavaFileSystem(javaFileSystem: => JavaFileSystem): HTaskLayer[FileSystem] =
    ZLayer.fromZIO(ZIO.hAttempt(WrappedJavaFileSystem(javaFileSystem)))

  def defaultJavaFileSystem: HTaskLayer[FileSystem] =
    wrapJavaFileSystem(JavaFileSystems.getDefault)

  override val liveLayer: HTaskLayer[FileSystem] =
    defaultJavaFileSystem

}
