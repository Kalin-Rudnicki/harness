package harness.zio

import harness.core.*
import java.nio.file.FileSystem as JavaFileSystem
import java.nio.file.FileSystems as JavaFileSystems
import zio.*

trait FileSystemCompanionPlatformSpecificImpl { self: FileSystemCompanionPlatformSpecific =>

  def wrapJavaFileSystem(javaFileSystem: => JavaFileSystem): Layer[HError, FileSystem] =
    ZLayer.fromZIO(ZIO.hAttempt("Unable to create filesystem")(WrappedJavaFileSystem(javaFileSystem)))

  def defaultJavaFileSystem: Layer[HError, FileSystem] =
    wrapJavaFileSystem(JavaFileSystems.getDefault)

  override val liveLayer: Layer[HError, FileSystem] = defaultJavaFileSystem

}
