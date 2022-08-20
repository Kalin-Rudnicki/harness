package harness.web.client.vdom

import harness.web.client.rawVDOM.VDom
import org.scalajs.dom.*
import scala.scalajs.js

abstract class KeyAttrBuilder[T](name: String, convert: T => js.Any) {
  final def :=(value: T): CModifier = PModifier(VDom.KeyAttr(name, convert(value)))
}

object onClick extends KeyAttrBuilder[MouseEvent => Unit]("onclick", identity(_))
object onDblClick extends KeyAttrBuilder[MouseEvent => Unit]("ondblclick", identity(_))
object onContextMenu extends KeyAttrBuilder[MouseEvent => Unit]("oncontextmenu", identity(_))
object onMouseDown extends KeyAttrBuilder[MouseEvent => Unit]("onmousedown", identity(_))
object onMouseUp extends KeyAttrBuilder[MouseEvent => Unit]("onmouseup", identity(_))
object onMouseOver extends KeyAttrBuilder[MouseEvent => Unit]("onmouseover", identity(_))
object onMouseMove extends KeyAttrBuilder[MouseEvent => Unit]("onmousemove", identity(_))
object onMouseOut extends KeyAttrBuilder[MouseEvent => Unit]("onmouseout", identity(_))
object onMouseEnter extends KeyAttrBuilder[MouseEvent => Unit]("onmouseenter", identity(_))
object onMouseLeave extends KeyAttrBuilder[MouseEvent => Unit]("onmouseleave", identity(_))

object onKeyDown extends KeyAttrBuilder[KeyboardEvent => Unit]("onkeydown", identity(_))
object onKeyUp extends KeyAttrBuilder[KeyboardEvent => Unit]("onkeyup", identity(_))
object onKeyPress extends KeyAttrBuilder[KeyboardEvent => Unit]("onkeypress", identity(_))

object onChange extends KeyAttrBuilder[Event => Unit]("onchange", identity(_))
object onSubmit extends KeyAttrBuilder[Event => Unit]("onsubmit", identity(_))
object onReset extends KeyAttrBuilder[Event => Unit]("onreset", identity(_))

object onFocus extends KeyAttrBuilder[FocusEvent => Unit]("onfocus", identity(_))
object onBlur extends KeyAttrBuilder[FocusEvent => Unit]("onblur", identity(_))

object onDrag extends KeyAttrBuilder[DragEvent => Unit]("ondrag", identity(_))
object onDragStart extends KeyAttrBuilder[DragEvent => Unit]("ondragstart", identity(_))
object onDragEnd extends KeyAttrBuilder[DragEvent => Unit]("ondragend", identity(_))
object onDragEnter extends KeyAttrBuilder[DragEvent => Unit]("ondragenter", identity(_))
object onDragLeave extends KeyAttrBuilder[DragEvent => Unit]("ondragleave", identity(_))
object onDragOver extends KeyAttrBuilder[DragEvent => Unit]("ondragover", identity(_))
object onDrop extends KeyAttrBuilder[DragEvent => Unit]("ondrop", identity(_))

object value extends KeyAttrBuilder[String]("value", identity(_))
