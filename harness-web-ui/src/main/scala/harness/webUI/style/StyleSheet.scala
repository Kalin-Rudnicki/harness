package harness.webUI.style

import harness.core.*
import harness.webUI.rawVDOM.VDom.{ClassName, CSSAttr}
import harness.webUI.vdom.{given, *}
import harness.zio.*
import scala.collection.mutable
import zio.*

abstract class StyleSheet(final val name: String)(inTags: StyleElement.InTag*) { styleSheet =>

  private[StyleSheet] final val blocks: mutable.ListBuffer[Block] = mutable.ListBuffer()

  abstract class Block(name: String)(styleElements: StyleElement*)
      extends StyleSheet.StyleSheetPart(
        name,
        s".$name",
        ClassName.b(name),
        styleElements,
      ) { block =>
    styleSheet.blocks.append(block)

    private[StyleSheet] final val elements: mutable.ListBuffer[Element] = mutable.ListBuffer()
    private[StyleSheet] final val modifiers: mutable.ListBuffer[Modifier] = mutable.ListBuffer()

    override protected def nested: List[mutable.ListBuffer[_ <: StyleSheet.StyleSheetPart]] = List(block.elements, block.modifiers)

    abstract class Element(name: String)(styleElements: StyleElement*)
        extends StyleSheet.StyleSheetPart(
          name,
          s".${block.name}__$name",
          ClassName.be(block.name, name),
          styleElements,
        ) { element =>
      block.elements.append(element)

      private[StyleSheet] final val modifiers: mutable.ListBuffer[Modifier] = mutable.ListBuffer()

      override protected def nested: List[mutable.ListBuffer[_ <: StyleSheet.StyleSheetPart]] = List(element.modifiers)

      abstract class Modifier(name: String)(styleElements: StyleElement*)
          extends StyleSheet.StyleSheetPart(
            name,
            s".${block.name}__${element.name}--$name",
            ClassName.bem(block.name, element.name, name),
            styleElements,
          ) { modifier =>
        element.modifiers.append(modifier)

        override protected def nested: List[mutable.ListBuffer[_ <: StyleSheet.StyleSheetPart]] = Nil

        final def when(cond: Boolean): ClassName = ClassName.bem(block.name, element.name, Option.when(cond)(modifier.name))

      }

      final def mod(mods: (this.type => Modifier)*): ClassName =
        ClassName.bem(block.name, element.name, mods.map(_(this).name))

    }

    abstract class Modifier(name: String)(styleElements: StyleElement*)
        extends StyleSheet.StyleSheetPart(
          name,
          s".${block.name}--$name",
          ClassName.bm(block.name, name),
          styleElements,
        ) { modifier =>
      block.modifiers.append(modifier)

      override protected def nested: List[mutable.ListBuffer[_ <: StyleSheet.StyleSheetPart]] = Nil

      final def when(cond: Boolean): ClassName = ClassName.bm(block.name, Option.when(cond)(modifier.name))

    }

  }

  final def toCssClassMap: CssClassMap =
    CssClassMap.mergeAll(
      inTags.toList.map { case StyleElement.InTag(tagName, children) => StyleSheet.expand(tagName, children) } :::
        blocks.toList.map(_.toCssClassMap),
    )

  override final def toString: String =
    styleSheet.toCssClassMap.renderOpt.getOrElse("/* empty css */")

}
object StyleSheet {

  private def expand(baseName: String, styleElement: StyleElement): CssClassMap =
    styleElement match {
      case StyleElement.Attr(attr)                               => CssClassMap.single(baseName, attr :: Nil)
      case StyleElement.Many(children)                           => expand(baseName, children)
      case StyleElement.InPseudoClass(pseudoClassName, children) => expand(s"$baseName:$pseudoClassName", children)
      case StyleElement.InTag(tagName, children)                 => expand(s"$baseName $tagName", children)
    }
  private def expand(baseName: String, styleElements: List[StyleElement]): CssClassMap =
    CssClassMap.mergeAll(styleElements.map(expand(baseName, _)))

  sealed abstract class StyleSheetPart(
      final val name: String,
      final val className: String,
      final val classNames: ClassName,
      _styleElements: Seq[StyleElement],
  ) {
    final val styleElements: List[StyleElement] = _styleElements.toList

    protected def nested: List[mutable.ListBuffer[_ <: StyleSheetPart]]

    private[StyleSheet] final def toCssClassMap: CssClassMap =
      CssClassMap.mergeAll(
        expand(className, styleElements) ::
          nested.flatMap(_.toList.map(_.toCssClassMap)),
      )

  }

}
