package harness.zio

import cats.syntax.option.*
import harness.zio.error.FSError
import java.nio.file.Files as JavaFiles
import java.nio.file.Path as JavaPath
import java.nio.file.attribute.FileTime as JavaFileTime
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

  override def optParent: IO[FSError.FailedToGetParent, Option[Path]] =
    ZIO.attempt(Option(javaPath.getParent).map(WrappedJavaPath.apply)).mapError(FSError.FailedToGetParent(show, _))

  override def createFile: IO[FSError.FailedToCreateFile, Unit] = ZIO.attempt(JavaFiles.createFile(javaPath)).mapError(FSError.FailedToCreateFile(show, _)).unit
  override def mkdir: IO[FSError.FailedToCreateDirectory, Unit] = ZIO.attempt(JavaFiles.createDirectory(javaPath)).mapError(FSError.FailedToCreateDirectory(show, _)).unit
  override def mkdirs: IO[FSError.FailedToCreateDirectories, Unit] = ZIO.attempt(JavaFiles.createDirectories(javaPath)).mapError(FSError.FailedToCreateDirectories(show, _)).unit
  override def createSymLink(target: Path): IO[FSError.FailedToCreateSymLink, Unit] =
    toJavaPath(target).flatMap { target =>
      ZIO.attempt(JavaFiles.createSymbolicLink(javaPath, target.javaPath)).mapError(FSError.FailedToCreateSymLink(show, target.show, _)).unit
    }
  override def createLink(existing: Path): IO[FSError.FailedToCreateLink, Unit] =
    toJavaPath(existing).flatMap { existing =>
      ZIO.attempt(JavaFiles.createLink(javaPath, existing.javaPath)).mapError(FSError.FailedToCreateLink(show, existing.show, _)).unit
    }

  override def delete: IO[FSError.FailedToDelete, Unit] = ZIO.attempt(JavaFiles.delete(javaPath)).mapError(FSError.FailedToDelete(show, _))
  override def deleteIfExists: IO[FSError.FailedToDelete, Boolean] = ZIO.attempt(JavaFiles.deleteIfExists(javaPath)).mapError(FSError.FailedToDelete(show, _))

  override def exists: IO[FSError.UnableToGetProperty, Boolean] = ZIO.attempt(JavaFiles.exists(javaPath)).mapError(FSError.UnableToGetProperty(show, "exists", _))

  override def isFile: IO[FSError.UnableToGetProperty, Boolean] = ZIO.attempt(JavaFiles.isRegularFile(javaPath)).mapError(FSError.UnableToGetProperty(show, "isFile", _))
  override def isDirectory: IO[FSError.UnableToGetProperty, Boolean] = ZIO.attempt(JavaFiles.isDirectory(javaPath)).mapError(FSError.UnableToGetProperty(show, "isDirectory", _))
  override def isSymbolicLink: IO[FSError.UnableToGetProperty, Boolean] = ZIO.attempt(JavaFiles.isSymbolicLink(javaPath)).mapError(FSError.UnableToGetProperty(show, "isSymbolicLink", _))

  override def getLastModifiedTime: IO[FSError.UnableToGetProperty, Long] =
    ZIO.attempt(JavaFiles.getLastModifiedTime(javaPath)).mapBoth(FSError.UnableToGetProperty(show, "lastModifiedTime", _), _.toMillis)
  override def setLastModifiedTime(millis: Long): IO[FSError.UnableToSetProperty, Unit] =
    ZIO.attempt(JavaFiles.setLastModifiedTime(javaPath, JavaFileTime.fromMillis(millis))).mapError(FSError.UnableToSetProperty(show, "lastModifiedTime", _)).unit
  override def size: IO[FSError.UnableToGetProperty, Long] =
    ZIO.attempt(JavaFiles.size(javaPath)).mapError(FSError.UnableToGetProperty(show, "size", _))

  override def child(childPath: String): IO[FSError.FailedToGetChild, Path] =
    ZIO.attempt(WrappedJavaPath(javaPath.resolve(childPath))).mapError(FSError.FailedToGetChild(show, childPath, _))
  override def children: IO[FSError.FailedToGetChildren, Chunk[Path]] =
    ZIO
      .attempt(JavaFiles.list(javaPath))
      .mapBoth(
        FSError.FailedToGetChildren(show, _),
        stream => Chunk.fromJavaIterator(stream.iterator).map(WrappedJavaPath(_)),
      )

  override def writeBytes(bytes: Array[Byte]): IO[FSError.UnableToWriteToFile, Unit] = ZIO.attempt(JavaFiles.write(javaPath, bytes)).mapError(FSError.UnableToWriteToFile(show, _)).unit
  override def writeString(string: String): IO[FSError.UnableToWriteToFile, Unit] = ZIO.attempt(JavaFiles.writeString(javaPath, string)).mapError(FSError.UnableToWriteToFile(show, _)).unit

  override def readBytes: IO[FSError.UnableToReadFromFile, Array[Byte]] = ZIO.attempt(JavaFiles.readAllBytes(javaPath)).mapError(FSError.UnableToReadFromFile(show, _))
  override def readString: IO[FSError.UnableToReadFromFile, String] = ZIO.attempt(JavaFiles.readString(javaPath)).mapError(FSError.UnableToReadFromFile(show, _))

  override def outputStream: ZIO[Scope, FSError.UnableToCreateOutputStream, java.io.OutputStream] =
    ZIO.acquireClosable { ZIO.attempt(JavaFiles.newOutputStream(javaPath)).mapError(FSError.UnableToCreateOutputStream(show, _)) }
  override def inputStream: ZIO[Scope, FSError.UnableToCreateInputStream, java.io.InputStream] =
    ZIO.acquireClosable { ZIO.attempt(JavaFiles.newInputStream(javaPath)).mapError(FSError.UnableToCreateInputStream(show, _)) }

  // =====| Helpers |=====

  private def toJavaPath(path: Path): UIO[WrappedJavaPath] =
    path match {
      case path: WrappedJavaPath => ZIO.succeed(path)
      case _                     => ZIO.dieMessage(s"not a JavaPath: ${path.getClass.getName} : ${path.show}")
    }

}
