package harness.zio

import harness.core.*
import harness.core.HError.ErrorType
import zio.*
import zio.json.{JsonDecoder, JsonEncoder}

trait Path {

  def show: String
  def absolutePath: Path
  def canonicalPath: Path
  def pathName: Path.Name

  override final def toString: String = show

  // =====| Without ErrorType |=====

  inline final def createFile: HTask[Unit] = createFile(ErrorType.SystemFailure)
  inline final def createFileIfDNE: HTask[Unit] = createFileIfDNE(ErrorType.SystemFailure)
  inline final def mkdir: HTask[Unit] = mkdir(ErrorType.SystemFailure)
  inline final def mkdirs: HTask[Unit] = mkdirs(ErrorType.SystemFailure)
  inline final def createSymLink(target: Path): HTask[Unit] = createSymLink(target, ErrorType.SystemFailure)
  inline final def createLink(existing: Path): HTask[Unit] = createLink(existing, ErrorType.SystemFailure)

  inline final def delete: HTask[Unit] = delete(ErrorType.SystemFailure)
  inline final def deleteIfExists: HTask[Boolean] = deleteIfExists(ErrorType.SystemFailure)

  inline final def exists: HTask[Boolean] = exists(ErrorType.SystemFailure)
  inline final def ensureExists: HTask[Unit] = ensureExists(ErrorType.SystemFailure)

  inline final def isFile: HTask[Boolean] = isFile(ErrorType.SystemFailure)
  inline final def isDirectory: HTask[Boolean] = isDirectory(ErrorType.SystemFailure)
  inline final def isSymbolicLink: HTask[Boolean] = isSymbolicLink(ErrorType.SystemFailure)

  inline final def getLastModifiedTime: HTask[Long] = getLastModifiedTime(ErrorType.SystemFailure)
  inline final def setLastModifiedTime(millis: Long): HTask[Unit] = setLastModifiedTime(millis, ErrorType.SystemFailure)
  inline final def size: HTask[Long] = size(ErrorType.SystemFailure)

  inline final def child(path: String): HTask[Path] = child(path, ErrorType.SystemFailure)
  inline final def children: HTask[Array[Path]] = children(ErrorType.SystemFailure)

  inline final def writeBytes(bytes: Array[Byte]): HTask[Unit] = writeBytes(bytes, ErrorType.SystemFailure)
  inline final def writeString(string: String): HTask[Unit] = writeString(string, ErrorType.SystemFailure)
  inline final def writeJson[T](t: T)(implicit encoder: JsonEncoder[T]): HTask[Unit] = writeJson[T](t, ErrorType.SystemFailure)

  inline final def readBytes: HTask[Array[Byte]] = readBytes(ErrorType.SystemFailure)
  inline final def readString: HTask[String] = readString(ErrorType.SystemFailure)
  inline final def readJson[T](implicit decoder: JsonDecoder[T]): HTask[T] = readJson[T](ErrorType.SystemFailure)

  inline final def outputStream: HRIO[Scope, java.io.OutputStream] = outputStream(ErrorType.SystemFailure)
  inline final def inputStream: HRIO[Scope, java.io.InputStream] = inputStream(ErrorType.SystemFailure)

  // =====| With ErrorType |=====

  def createFile(errorType: => HError.ErrorType): HTask[Unit]
  inline final def createFileIfDNE(errorType: => HError.ErrorType): HTask[Unit] =
    exists(errorType).flatMap {
      case false => createFile(errorType)
      case true  => ZIO.unit
    }
  def mkdir(errorType: => HError.ErrorType): HTask[Unit]
  def mkdirs(errorType: => HError.ErrorType): HTask[Unit]
  def createSymLink(target: Path, errorType: => HError.ErrorType): HTask[Unit]
  def createLink(existing: Path, errorType: => HError.ErrorType): HTask[Unit]

  def delete(errorType: => HError.ErrorType): HTask[Unit]
  def deleteIfExists(errorType: => HError.ErrorType): HTask[Boolean]

  def exists(errorType: => HError.ErrorType): HTask[Boolean]
  inline final def ensureExists(errorType: => HError.ErrorType): HTask[Unit] =
    exists(errorType).flatMap {
      case true  => ZIO.unit
      case false => ZIO.fail(HError(errorType)(s"File that should exist does not : $show"))
    }

  def isFile(errorType: => HError.ErrorType): HTask[Boolean]
  def isDirectory(errorType: => HError.ErrorType): HTask[Boolean]
  def isSymbolicLink(errorType: => HError.ErrorType): HTask[Boolean]

  def getLastModifiedTime(errorType: => HError.ErrorType): HTask[Long]
  def setLastModifiedTime(millis: Long, errorType: => HError.ErrorType): HTask[Unit]
  def size(errorType: => HError.ErrorType): HTask[Long]

  def child(childPath: String, errorType: => HError.ErrorType): HTask[Path]
  def children(errorType: => HError.ErrorType): HTask[Array[Path]]

  def writeBytes(bytes: Array[Byte], errorType: => HError.ErrorType): HTask[Unit]
  def writeString(string: String, errorType: => HError.ErrorType): HTask[Unit]
  inline final def writeJson[T](t: T, errorType: => HError.ErrorType)(implicit encoder: JsonEncoder[T]): HTask[Unit] =
    writeString(encoder.encodeJson(t, None).toString, errorType)

  def readBytes(errorType: => HError.ErrorType): HTask[Array[Byte]]
  def readString(errorType: => HError.ErrorType): HTask[String]
  inline final def readJson[T](errorType: => HError.ErrorType)(implicit decoder: JsonDecoder[T]): HTask[T] =
    readString(errorType).flatMap {
      decoder.decodeJson(_) match {
        case Right(value) => ZIO.succeed(value)
        case Left(error)  => ZIO.fail(HError(errorType)(s"Unable to decode json:\n$error"))
      }
    }

  def outputStream(errorType: => HError.ErrorType): HRIO[Scope, java.io.OutputStream]
  def inputStream(errorType: => HError.ErrorType): HRIO[Scope, java.io.InputStream]

}
object Path {

  inline def apply(string: String): HRIO[FileSystem, Path] = FileSystem.path(string)

  final case class Name(
      name: String,
      base: String,
      ext: Option[String],
  ) {
    override def toString: String = name
  }

}
