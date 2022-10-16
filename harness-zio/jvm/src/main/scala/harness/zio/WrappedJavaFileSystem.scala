package harness.zio

import java.nio.file.FileSystem as JavaFileSystem
import scala.jdk.CollectionConverters.*
import zio.*

final case class WrappedJavaFileSystem(javaFileSystem: JavaFileSystem) extends FileSystem {
  override def path(string: String): HTask[Path] = ZIO.hAttempt(WrappedJavaPath(javaFileSystem.getPath(string)))
  override def homeDirectory: HTask[Path] = ZIO.hAttempt(java.lang.System.getProperty("user.home")).flatMap(path)
  override def roots: HTask[Array[Path]] = ZIO.hAttempt(javaFileSystem.getRootDirectories.asScala.toArray.map(WrappedJavaPath(_)))
}
