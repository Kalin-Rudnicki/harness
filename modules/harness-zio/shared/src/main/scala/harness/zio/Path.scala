package harness.zio

import harness.zio.error.FSError
import zio.*
import zio.json.{JsonDecoder, JsonEncoder}

trait Path {

  def show: String
  def absolutePath: Path
  def canonicalPath: Path
  def pathName: Path.Name

  override final def toString: String = show

  def optParent: IO[FSError.FailedToGetParent, Option[Path]]
  inline final def parent: IO[FSError.FailedToGetParent | FSError.PathParentDNE, Path] =
    optParent.someOrFail(FSError.PathParentDNE(show))

  def createFile: IO[FSError.FailedToCreateFile, Unit]
  def mkdir: IO[FSError.FailedToCreateDirectory, Unit]
  def mkdirs: IO[FSError.FailedToCreateDirectories, Unit]
  def createSymLink(target: Path): IO[FSError.FailedToCreateSymLink, Unit]
  def createLink(existing: Path): IO[FSError.FailedToCreateLink, Unit]

  def delete: IO[FSError.FailedToDelete, Unit]
  def deleteIfExists: IO[FSError.FailedToDelete, Boolean]

  def exists: IO[FSError.UnableToGetProperty, Boolean]

  def isFile: IO[FSError.UnableToGetProperty, Boolean]
  def isDirectory: IO[FSError.UnableToGetProperty, Boolean]
  def isSymbolicLink: IO[FSError.UnableToGetProperty, Boolean]

  inline final def createFileIfDNE: IO[FSError.FailedToCreateFile | FSError.UnableToGetProperty, Unit] =
    createFile.unlessZIODiscard(exists)
  inline final def ensureExists: IO[FSError.UnableToGetProperty | FSError.PathDNE, Unit] =
    ZIO.fail(FSError.PathDNE(show)).unlessZIODiscard(exists)
  inline final def ensureIsFile: IO[FSError.UnableToGetProperty | FSError.PathDNE | FSError.PathIsNotAFile, Unit] =
    ensureExists *>
      ZIO.fail(FSError.PathIsNotAFile(show)).unlessZIODiscard(isFile)
  inline final def ensureIsDirectory: IO[FSError.UnableToGetProperty | FSError.PathDNE | FSError.PathIsNotADirectory, Unit] =
    ensureExists *>
      ZIO.fail(FSError.PathIsNotADirectory(show)).unlessZIODiscard(isDirectory)

  def getLastModifiedTime: IO[FSError.UnableToGetProperty, Long]
  def setLastModifiedTime(millis: Long): IO[FSError.UnableToSetProperty, Unit]
  def size: IO[FSError.UnableToGetProperty, Long]

  def child(childPath: String): IO[FSError.FailedToGetChild, Path]
  def children: IO[FSError.FailedToGetChildren, Chunk[Path]]

  def writeBytes(bytes: Array[Byte]): IO[FSError.UnableToWriteToFile, Unit]
  def writeString(string: String): IO[FSError.UnableToWriteToFile, Unit]
  inline final def writeJson[T](t: T)(implicit encoder: JsonEncoder[T]): IO[FSError.UnableToWriteToFile, Unit] =
    writeString(encoder.encodeJson(t, None).toString)

  def readBytes: IO[FSError.UnableToReadFromFile, Array[Byte]]
  def readString: IO[FSError.UnableToReadFromFile, String]
  inline final def readJson[T](implicit decoder: JsonDecoder[T]): IO[FSError.UnableToReadFromFile | FSError.UnableToDecodeFileContents, T] =
    readString.flatMap {
      decoder.decodeJson(_) match {
        case Right(value) => ZIO.succeed(value)
        case Left(error)  => ZIO.fail(FSError.UnableToDecodeFileContents(error, show))
      }
    }

  def outputStream: ZIO[Scope, FSError.UnableToCreateOutputStream, java.io.OutputStream]
  def inputStream: ZIO[Scope, FSError.UnableToCreateInputStream, java.io.InputStream]

}
object Path {

  inline def apply(string: String): IO[FSError.UnableToResolvePath, Path] = FileSystem.path(string)

  final case class Name(
      name: String,
      base: String,
      ext: Option[String],
  ) {
    override def toString: String = name
  }

}
