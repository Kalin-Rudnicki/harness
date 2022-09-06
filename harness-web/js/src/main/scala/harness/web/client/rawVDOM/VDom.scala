package harness.web.client.rawVDOM

import cats.Monoid
import cats.syntax.option.*
import scala.annotation.tailrec
import scala.scalajs.js

object VDom {

  // =====| ScopedName |=====

  final case class ScopedName(prefix: Option[String], name: String) {
    override def toString: String = prefix.fold(name)(p => s"$p:$name")
  }
  object ScopedName {
    def apply(prefix: String, name: String): ScopedName = ScopedName(prefix.some, name)
    def apply(name: String): ScopedName = ScopedName(None, name)
  }

  // =====| Modifier |=====

  sealed trait Modifier {

    final def toBasics: List[Modifier.Basic] =
      this match {
        case basic: Modifier.Basic      => basic :: Nil
        case Modifier.Wrapped(children) => children
      }

  }
  object Modifier {

    sealed trait Basic extends Modifier
    final case class Wrapped(children: List[Basic]) extends Modifier

    def apply(children: Modifier*): Modifier = Wrapped(children.toList.flatMap(_.toBasics))
    def flatten(children: List[Modifier]): Modifier = Wrapped(children.flatMap(_.toBasics))

    val empty: Modifier = Wrapped(Nil)

  }

  // =====| Element |=====

  sealed trait Element extends Modifier.Basic {
    final def nodeName: String =
      this match {
        case NodeElement(tagName, _) => tagName.toUpperCase
        case _: TextElement          => "#text"
      }
  }

  final case class TextElement(text: String) extends Element

  final case class NodeElement(
      tagName: String,
      modifiers: List[Modifier.Basic],
  ) extends Element {

    def splitModifiers: (List[Element], Set[String], Map[ScopedName, String], Map[ScopedName, String], Map[String, js.Any]) = {
      @tailrec
      def loop(
          queue: List[Modifier.Basic],
          elements: List[Element],
          classNames: Set[String],
          cssAttrs: Map[ScopedName, String],
          stdAttrs: Map[ScopedName, String],
          objAttrs: Map[String, js.Any],
      ): (List[Element], Set[String], Map[ScopedName, String], Map[ScopedName, String], Map[String, js.Any]) =
        queue match {
          case head :: tail =>
            head match {
              case element: Element           => loop(tail, element :: elements, classNames, cssAttrs, stdAttrs, objAttrs)
              case name: ClassName            => loop(tail, elements, classNames ++ name.classNames, cssAttrs, stdAttrs, objAttrs)
              case CSSAttr(scopedName, value) => loop(tail, elements, classNames, cssAttrs.updated(scopedName, value), stdAttrs, objAttrs)
              case StdAttr(scopedName, value) => loop(tail, elements, classNames, cssAttrs, stdAttrs.updated(scopedName, value), objAttrs)
              case KeyAttr(name, value)       => loop(tail, elements, classNames, cssAttrs, stdAttrs, objAttrs.updated(name, value))
            }
          case Nil => (elements.reverse, classNames, cssAttrs, stdAttrs, objAttrs)
        }

      loop(modifiers, Nil, Set.empty, Map.empty, Map.empty, Map.empty)
    }

  }
  object NodeElement {
    def apply(nodeName: String): NodeElement = NodeElement(nodeName, Nil)
  }

  // =====| Attrs |=====

  enum ClassName extends Modifier.Basic {
    case Block(block: String, modifiers: Set[String])
    case Element(block: String, element: String, modifiers: Set[String])

    final def classNames: Set[String] =
      this match {
        case Block(block, modifiers)            => ClassName.classNames(block, modifiers)
        case Element(block, element, modifiers) => ClassName.classNames(s"${block}__$element", modifiers)
      }

  }

  object ClassName {

    private def classNames(base: String, modifiers: Set[String]): Set[String] = modifiers.map(modifier => s"$base--$modifier") + base

    def b(block: String): ClassName = Block(block, Set.empty)
    def b(block: String, m0: String, mN: String*): ClassName = Block(block, (m0 :: mN.toList).toSet)
    def b(block: String, m0: IterableOnce[String], mN: IterableOnce[String]*): ClassName = Block(block, (m0 :: mN.toList).toSet.flatten)

    def be(block: String, element: String): ClassName = Element(block, element, Set.empty)
    def be(block: String, element: String, m0: String, mN: String*): ClassName = Element(block, element, (m0 :: mN.toList).toSet)
    def be(block: String, element: String, m0: IterableOnce[String], mN: IterableOnce[String]*): ClassName = Element(block, element, (m0 :: mN.toList).toSet.flatten)

  }

  final case class CSSAttr(scopedName: ScopedName, value: String) extends Modifier.Basic
  final case class StdAttr(scopedName: ScopedName, value: String) extends Modifier.Basic
  final case class KeyAttr(name: String, value: js.Any) extends Modifier.Basic

}