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

  override def createFile(errorType: => HError.ErrorType): HTask[Unit] = ZIO.hAttempt(errorType, s"Unable to create file : $show")(JavaFiles.createFile(javaPath))
  override def mkdir(errorType: => HError.ErrorType): HTask[Unit] = ZIO.hAttempt(errorType, s"Unable to create directory : $show")(JavaFiles.createDirectory(javaPath))
  override def mkdirs(errorType: => HError.ErrorType): HTask[Unit] = ZIO.hAttempt(errorType, s"Unable to create directories : $show")(JavaFiles.createDirectories(javaPath))
  override def createSymLink(target: Path, errorType: => HError.ErrorType): HTask[Unit] =
    target match {
      case target: WrappedJavaPath => ZIO.hAttempt(errorType, s"Unable to create sym-link : $show")(JavaFiles.createSymbolicLink(javaPath, target.javaPath))
      case _                       => ZIO.fail(HError.InternalDefect("Tried to create sym-link with non-java-file"))
    }
  override def createLink(existing: Path, errorType: => HError.ErrorType): HTask[Unit] =
    existing match {
      case existing: WrappedJavaPath => ZIO.hAttempt(errorType, s"Unable to create link : $show")(JavaFiles.createLink(javaPath, existing.javaPath))
      case _                         => ZIO.fail(HError.InternalDefect("Tried to create link with non-java-file"))
    }

  override def delete(errorType: => HError.ErrorType): HTask[Unit] = ZIO.hAttempt(errorType, s"Unable to delete file : $show")(JavaFiles.delete(javaPath))
  override def deleteIfExists(errorType: => HError.ErrorType): HTask[Boolean] = ZIO.hAttempt(errorType, s"Unable to delete file if exists : $show")(JavaFiles.deleteIfExists(javaPath))

  override def exists(errorType: => HError.ErrorType): HTask[Boolean] = ZIO.hAttempt(errorType, s"Unable to check if file exists : $show")(JavaFiles.exists(javaPath))

  override def isFile(errorType: => HError.ErrorType): HTask[Boolean] = ZIO.hAttempt(errorType, s"Unable to check if file is file : $show")(JavaFiles.isRegularFile(javaPath))
  override def isDirectory(errorType: => HError.ErrorType): HTask[Boolean] = ZIO.hAttempt(errorType, s"Unable to check if file is directory : $show")(JavaFiles.isDirectory(javaPath))
  override def isSymbolicLink(errorType: => HError.ErrorType): HTask[Boolean] = ZIO.hAttempt(errorType, s"Unable to check if file is sym-link : $show")(JavaFiles.isSymbolicLink(javaPath))

  override def getLastModifiedTime(errorType: => HError.ErrorType): HTask[Long] =
    ZIO.hAttempt(errorType, s"Unable to get last modified time : $show")(JavaFiles.getLastModifiedTime(javaPath)).map(_.toMillis)
  override def setLastModifiedTime(millis: Long, errorType: => HError.ErrorType): HTask[Unit] =
    ZIO.hAttempt(errorType, s"Unable to set last modified time : $show")(JavaFiles.setLastModifiedTime(javaPath, JavaFileTime.fromMillis(millis))).unit
  override def size(errorType: => HError.ErrorType): HTask[Long] = ZIO.hAttempt(errorType, s"Unable to get file size : $show")(JavaFiles.size(javaPath))

  override def child(childPath: String, errorType: => HError.ErrorType): HTask[Path] =
    ZIO.hAttempt(errorType, s"Unable to create child instance of file : $show ($childPath)")(WrappedJavaPath(javaPath.resolve(childPath)))
  override def children(errorType: => HError.ErrorType): HTask[Array[Path]] =
    ZIO
      .hAttempt(errorType, s"Unable to get children of file : $show")(JavaFiles.list(javaPath))
      .map { _.iterator.asScala.toArray.map(WrappedJavaPath(_)) }

  override def writeBytes(bytes: Array[Byte], errorType: => HError.ErrorType): HTask[Unit] = ZIO.hAttempt(errorType, s"Unable to write bytes to file : $show")(JavaFiles.write(javaPath, bytes))
  override def writeString(string: String, errorType: => HError.ErrorType): HTask[Unit] = ZIO.hAttempt(errorType, s"Unable to write string to file : $show")(JavaFiles.writeString(javaPath, string))

  override def readBytes(errorType: => HError.ErrorType): HTask[Array[Byte]] = ZIO.hAttempt(errorType, s"Unable to read bytes from file : $show")(JavaFiles.readAllBytes(javaPath))
  override def readString(errorType: => HError.ErrorType): HTask[String] = ZIO.hAttempt(errorType, s"Unable to read string from file : $show")(JavaFiles.readString(javaPath))

  override def outputStream(errorType: => HError.ErrorType): HRIO[Scope, java.io.OutputStream] =
    ZIO.acquireClosable {
      ZIO.hAttempt(errorType, s"Unable to get OutputStream for file : $show")(JavaFiles.newOutputStream(javaPath))
    }
  override def inputStream(errorType: => HError.ErrorType): HRIO[Scope, java.io.InputStream] =
    ZIO.acquireClosable {
      ZIO.hAttempt(errorType, s"Unable to get InputStream for file : $show")(JavaFiles.newInputStream(javaPath))
    }

}
