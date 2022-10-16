package harness.zio

import cats.syntax.option.*
import harness.core.*
import java.nio.file.Files as JavaFiles
import java.nio.file.Path as JavaPath
import java.nio.file.attribute.FileTime as JavaFileTime
import scala.jdk.CollectionConverters.*
import zio.*

final case class WrappedJavaPath(javaPath: JavaPath) extends Path {

  override def show: String = javaPath.toString
  override def absolutePath: Path = WrappedJavaPath(javaPath.toAbsolutePath)
  override def canonicalPath: Path = WrappedJavaPath(javaPath.normalize)
  override def pathName: Path.Name = {
    val name = javaPath.getFileName.toString
    name.lastIndexOf('.') match {
      case -1  => Path.Name(name, name, None)
      case idx => Path.Name(name, name.substring(0, idx), name.substring(idx + 1).some)
    }
  }

  override def createFile: HTask[Unit] = ZIO.hAttempt(JavaFiles.createFile(javaPath))
  override def mkdir: HTask[Unit] = ZIO.hAttempt(JavaFiles.createDirectory(javaPath))
  override def mkdirs: HTask[Unit] = ZIO.hAttempt(JavaFiles.createDirectories(javaPath))
  override def createSymLink(target: Path): HTask[Unit] =
    target match {
      case target: WrappedJavaPath => ZIO.hAttempt(JavaFiles.createSymbolicLink(javaPath, target.javaPath))
      case _                       => ZIO.fail(HError.InternalDefect("Tried to create sym-link with non-java-file"))
    }
  override def createLink(existing: Path): HTask[Unit] =
    existing match {
      case existing: WrappedJavaPath => ZIO.hAttempt(JavaFiles.createLink(javaPath, existing.javaPath))
      case _                         => ZIO.fail(HError.InternalDefect("Tried to create link with non-java-file"))
    }

  override def delete: HTask[Unit] = ZIO.hAttempt(JavaFiles.delete(javaPath))
  override def deleteIfExists: HTask[Boolean] = ZIO.hAttempt(JavaFiles.deleteIfExists(javaPath))

  override def exists: HTask[Boolean] = ZIO.hAttempt(JavaFiles.exists(javaPath))

  override def isFile: HTask[Boolean] = ZIO.hAttempt(JavaFiles.isRegularFile(javaPath))
  override def isDirectory: HTask[Boolean] = ZIO.hAttempt(JavaFiles.isDirectory(javaPath))
  override def isSymbolicLink: HTask[Boolean] = ZIO.hAttempt(JavaFiles.isSymbolicLink(javaPath))

  override def getLastModifiedTime: HTask[Long] =
    ZIO.hAttempt(JavaFiles.getLastModifiedTime(javaPath)).map(_.toMillis)
  override def setLastModifiedTime(millis: Long): HTask[Unit] =
    ZIO.hAttempt(JavaFiles.setLastModifiedTime(javaPath, JavaFileTime.fromMillis(millis))).unit
  override def size: HTask[Long] = ZIO.hAttempt(JavaFiles.size(javaPath))

  override def child(childPath: String): HTask[Path] =
    ZIO.hAttempt(WrappedJavaPath(javaPath.resolve(childPath)))
  override def children: HTask[Array[Path]] =
    ZIO
      .hAttempt(JavaFiles.list(javaPath))
      .map { _.iterator.asScala.toArray.map(WrappedJavaPath(_)) }

  override def writeBytes(bytes: Array[Byte]): HTask[Unit] = ZIO.hAttempt(JavaFiles.write(javaPath, bytes))
  override def writeString(string: String): HTask[Unit] = ZIO.hAttempt(JavaFiles.writeString(javaPath, string))

  override def readBytes: HTask[Array[Byte]] = ZIO.hAttempt(JavaFiles.readAllBytes(javaPath))
  override def readString: HTask[String] = ZIO.hAttempt(JavaFiles.readString(javaPath))

  override def outputStream: HRIO[Scope, java.io.OutputStream] =
    ZIO.acquireClosable { ZIO.hAttempt(JavaFiles.newOutputStream(javaPath)) }
  override def inputStream: HRIO[Scope, java.io.InputStream] =
    ZIO.acquireClosable { ZIO.hAttempt(JavaFiles.newInputStream(javaPath)) }

}
