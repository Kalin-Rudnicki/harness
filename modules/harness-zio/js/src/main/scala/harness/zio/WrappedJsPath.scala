package harness.zio
import harness.zio.error.FSError
import java.io.{InputStream, OutputStream}
import zio.{Chunk, IO, Scope, ZIO}

final case class WrappedJsPath(file: org.scalajs.dom.File) extends Path { self =>

  override def show: String = file.toString

  override def absolutePath: Path = ???

  override def canonicalPath: Path = ???

  override def pathName: Path.Name = ???

  override def optParent: IO[FSError.FailedToGetParent, Option[Path]] = ???

  override def createFile: IO[FSError.FailedToCreateFile, Unit] = ???

  override def mkdir: IO[FSError.FailedToCreateDirectory, Unit] = ???

  override def mkdirs: IO[FSError.FailedToCreateDirectories, Unit] = ???

  override def createSymLink(target: Path): IO[FSError.FailedToCreateSymLink, Unit] = ???

  override def createLink(existing: Path): IO[FSError.FailedToCreateLink, Unit] = ???

  override def delete: IO[FSError.FailedToDelete, Unit] = ???

  override def deleteIfExists: IO[FSError.FailedToDelete, Boolean] = ???

  override def exists: IO[FSError.UnableToGetProperty, Boolean] = ???

  override def isFile: IO[FSError.UnableToGetProperty, Boolean] = ???

  override def isDirectory: IO[FSError.UnableToGetProperty, Boolean] = ???

  override def isSymbolicLink: IO[FSError.UnableToGetProperty, Boolean] = ???

  override def getLastModifiedTime: IO[FSError.UnableToGetProperty, Long] = ???

  override def setLastModifiedTime(millis: Long): IO[FSError.UnableToSetProperty, Unit] = ???

  override def size: IO[FSError.UnableToGetProperty, Long] = ???

  override def child(childPath: String): IO[FSError.FailedToGetChild, Path] = ???

  override def children: IO[FSError.FailedToGetChildren, Chunk[Path]] = ???

  override def writeBytes(bytes: Array[Byte]): IO[FSError.UnableToWriteToFile, Unit] = ???

  override def writeString(string: String): IO[FSError.UnableToWriteToFile, Unit] = ???

  override def readBytes: IO[FSError.UnableToReadFromFile, Array[Byte]] = ???

  override def readString: IO[FSError.UnableToReadFromFile, String] = ???

  override def outputStream: ZIO[Scope, FSError.UnableToCreateOutputStream, OutputStream] = ???

  override def inputStream: ZIO[Scope, FSError.UnableToCreateInputStream, InputStream] = ???

}
