package harness.web.client.rawVDOM

import cats.syntax.option.*
import harness.web.client.rawVDOM.VDom.*
import harness.zio.*
import org.scalajs.dom.{document, window, Element as DomElement, HTMLHtmlElement, Node}
import scala.annotation.tailrec
import scalajs.js.Dynamic
import zio.*

final class Renderer private (ref: Ref.Synchronized[Option[VDom.State]]) {

  def render(title: String, newVDoms: List[Modifier]): HTask[Unit] =
    ref.updateZIO { oldVDomState =>
      val runSetTitle = ZIO.hAttempt("Unable to set title of document")(window.document.title = title)
      val newVDomState = VDom.State.fromModifiers(newVDoms.flatMap(_.toBasics))
      val runRender =
        ZIO.hAttempt("Unable to (re)render VDOM") {
          oldVDomState match {
            case Some(oldVDomState) => Renderer.diffStates(document.body, newVDomState, oldVDomState)
            case None               => Renderer.setBody(newVDoms)
          }
        }

      runSetTitle *> runRender *> ZIO.some(newVDomState)
    }

}
object Renderer {

  val Initial: UIO[Renderer] = Ref.Synchronized.make(Option.empty[VDom.State]).map(Renderer(_))

  private def build(element: Element): Node =
    element match {
      case nodeElement: NodeElement =>
        val node = document.createElement(nodeElement.tagName)
        val nodeDynamic = node.asInstanceOf[Dynamic]
        val VDom.State(elements, classNames, cssAttrs, stdAttrs, objAttrs) = nodeElement.innerState

        setClassNames(node, classNames)
        setStyle(node, cssAttrs)
        stdAttrs.foreach { (name, value) => node.setAttribute(name.toString, value) }
        objAttrs.foreach { (key, value) =>
          nodeDynamic.updateDynamic(key)(value)
        }

        elements.foreach(element => node.appendChild(build(element)))

        node
      case textElement: TextElement =>
        document.createTextNode(textElement.text)
    }

  private def setClassNames(element: DomElement, classNames: Set[String]): Unit =
    if (classNames.nonEmpty) element.setAttribute("class", classNames.mkString(" "))
    else element.removeAttribute("class")

  private def setStyle(element: DomElement, cssAttrs: Map[ScopedName, String]): Unit =
    if (cssAttrs.nonEmpty) element.setAttribute("style", cssAttrs.toList.map { (sn, v) => s"$sn: $v" }.mkString("; "))
    else element.removeAttribute("style")

  @tailrec
  def loopElements(
      parent: Node,
      children: Array[Node],
      newVDoms: List[Element],
      oldVDoms: List[Element],
      idx: Int,
  ): Unit =
    (newVDoms, oldVDoms) match {
      case (newVDom :: newVDomTail, oldVDom :: oldVDomTail) =>
        (newVDom, oldVDom) match {
          case (newNodeElement: NodeElement, oldNodeElement: NodeElement) =>
            if (newNodeElement.tagName != oldNodeElement.tagName) parent.replaceChild(build(newNodeElement), children(idx))
            else
              diffStates(
                children(idx).asInstanceOf[DomElement],
                newNodeElement.innerState,
                oldNodeElement.innerState,
              )
          case (newTextElement: TextElement, oldTextElement: TextElement) =>
            if (newTextElement.text != oldTextElement.text)
              parent.replaceChild(build(newTextElement), children(idx))
          case (newVDom, _) =>
            parent.replaceChild(build(newVDom), children(idx))
        }
        loopElements(parent, children, newVDomTail, oldVDomTail, idx + 1)
      case (Nil, _ :: oldVDomTail) =>
        parent.removeChild(children(idx))
        loopElements(parent, children, Nil, oldVDomTail, idx + 1)
      case (newVDom :: newVDomTail, Nil) =>
        parent.appendChild(build(newVDom))
        loopElements(parent, children, newVDomTail, Nil, idx + 1)
      case (Nil, Nil) =>
    }

  private def diffStates(
      node: DomElement,
      newState: VDom.State,
      oldState: VDom.State,
  ): Unit = {
    val nodeDynamic = node.asInstanceOf[Dynamic]

    // --- Elements ---
    loopElements(
      node,
      (0 until node.childNodes.length).map(node.childNodes(_)).toArray,
      newState.elements,
      oldState.elements,
      0,
    )

    // --- Class Names ---
    if (newState.classNames != oldState.classNames)
      setClassNames(node, newState.classNames)

    // --- CSS Attrs ---
    if (newState.cssAttrs != oldState.cssAttrs)
      setStyle(node, newState.cssAttrs)

    // --- Std Attrs ---
    // TODO (KR) : use `*AttributeNS` instead?
    (newState.stdAttrs.keySet ++ oldState.stdAttrs.keySet).foreach { key =>
      (newState.stdAttrs.get(key), oldState.stdAttrs.get(key)) match {
        case (Some(newValue), Some(oldValue)) =>
          if (newValue != oldValue)
            node.setAttribute(key.toString, newValue)
        case (Some(newValue), None) =>
          node.setAttribute(key.toString, newValue)
        case (None, Some(_)) =>
          node.removeAttribute(key.toString)
        case (None, None) =>
      }
    }

    // --- Obj Attrs ---
    (newState.objAttrs.keySet ++ oldState.objAttrs.keySet).foreach { key =>
      (newState.objAttrs.get(key), oldState.objAttrs.get(key)) match {
        case (Some(newValue), Some(oldValue)) =>
          if (newValue != oldValue)
            nodeDynamic.updateDynamic(key)(newValue)
        case (Some(newValue), None) =>
          nodeDynamic.updateDynamic(key)(newValue)
        case (None, Some(_)) =>
          nodeDynamic.updateDynamic(key)(null)
        case (None, None) =>
      }
    }
  }

  private def setBody(elements: List[Modifier]): Unit =
    document.body = build(NodeElement("body", elements.flatMap(_.toBasics))).asInstanceOf[HTMLHtmlElement]

}
