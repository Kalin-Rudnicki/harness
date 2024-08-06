package harness.zio

import java.nio.file.FileSystems as JavaFileSystems

private[zio] trait FileSystemCompanionPlatformSpecificImpl { self: FileSystemCompanionPlatformSpecific =>

  override def defaultFS: FileSystem = WrappedJavaFileSystem(JavaFileSystems.getDefault)

}
