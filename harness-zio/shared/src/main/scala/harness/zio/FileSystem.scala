package harness.zio

import harness.core.*
import zio.*

trait FileSystem {
  def path(string: String): HTask[Path]
  def homeDirectory: HTask[Path]
  def roots: HTask[Array[Path]]
}
object FileSystem extends FileSystemCompanionPlatformSpecific with FileSystemCompanionPlatformSpecificImpl {

  def path(string: String): HRIO[FileSystem, Path] = ZIO.service[FileSystem].flatMap(_.path(string))
  def homeDirectory: HRIO[FileSystem, Path] = ZIO.service[FileSystem].flatMap(_.homeDirectory)
  def roots: HRIO[FileSystem, Array[Path]] = ZIO.service[FileSystem].flatMap(_.roots)

  case object Unimplemented extends FileSystem {
    def path(string: String): HTask[Path] = ZIO.fail(HError.???("FileSystem.path"))
    def homeDirectory: HTask[Path] = ZIO.fail(HError.???("FileSystem.homeDirectory"))
    def roots: HTask[Array[Path]] = ZIO.fail(HError.???("FileSystem.roots"))
  }

}
