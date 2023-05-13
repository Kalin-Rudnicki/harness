package harness.webUI.facades

import org.scalajs.dom.Window
import zio.*

import scala.scalajs.js
import scala.scalajs.js.annotation.*

@js.native
trait WindowOps extends js.Any {
  def showOpenFilePicker(opts: js.Any = js.native): js.Promise[js.Array[FileSystemFileHandle]] = js.native // TODO (KR) :
  def showDirectoryPicker(): js.Promise[FileSystemDirectoryHandle]
}

extension (window: Window) {
  def showOpenFilePicker(multiple: Boolean): UIO[Option[js.Array[FileSystemFileHandle]]] =
    ZIO.fromPromiseJS(window.asInstanceOf[WindowOps].showOpenFilePicker(js.Dictionary("multiple" -> multiple))).fold(_ => None, Option(_))
  def showDirectoryPicker: UIO[Option[FileSystemDirectoryHandle]] =
    ZIO.fromPromiseJS(window.asInstanceOf[WindowOps].showDirectoryPicker()).fold(_ => None, Option(_))
}
