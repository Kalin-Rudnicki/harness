package harness.zio

import harness.core.*
import zio.*
import zio.json.{JsonDecoder, JsonEncoder}

trait Path {

  def show: String
  def absolutePath: Path
  def canonicalPath: Path
  def pathName: Path.Name

  override final def toString: String = show

  def optParent: HTask[Option[Path]]
  inline final def parent: HTask[Path] =
    optParent.someOrFail(HError.UserError(s"Path does not have a parent : $show"))

  def createFile: HTask[Unit]
  inline final def createFileIfDNE: HTask[Unit] =
    exists.flatMap {
      case false => createFile
      case true  => ZIO.unit
    }
  def mkdir: HTask[Unit]
  def mkdirs: HTask[Unit]
  def createSymLink(target: Path): HTask[Unit]
  def createLink(existing: Path): HTask[Unit]

  def delete: HTask[Unit]
  def deleteIfExists: HTask[Boolean]

  def exists: HTask[Boolean]
  inline final def ensureExists: HTask[Unit] =
    exists.flatMap {
      case true  => ZIO.unit
      case false => ZIO.fail(HError.UserError(s"File that should exist does not : $show"))
    }

  def isFile: HTask[Boolean]
  def isDirectory: HTask[Boolean]
  def isSymbolicLink: HTask[Boolean]

  def getLastModifiedTime: HTask[Long]
  def setLastModifiedTime(millis: Long): HTask[Unit]
  def size: HTask[Long]

  def child(childPath: String): HTask[Path]
  def children: HTask[Array[Path]]

  def writeBytes(bytes: Array[Byte]): HTask[Unit]
  def writeString(string: String): HTask[Unit]
  inline final def writeJson[T](t: T)(implicit encoder: JsonEncoder[T]): HTask[Unit] =
    writeString(encoder.encodeJson(t, None).toString)

  def readBytes: HTask[Array[Byte]]
  def readString: HTask[String]
  inline final def readJson[T](implicit decoder: JsonDecoder[T]): HTask[T] =
    readString.flatMap {
      decoder.decodeJson(_) match {
        case Right(value) => ZIO.succeed(value)
        case Left(error)  => ZIO.fail(HError.UserError(s"Unable to decode json:\n$error"))
      }
    }

  def outputStream: HRIO[Scope, java.io.OutputStream]
  def inputStream: HRIO[Scope, java.io.InputStream]

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
