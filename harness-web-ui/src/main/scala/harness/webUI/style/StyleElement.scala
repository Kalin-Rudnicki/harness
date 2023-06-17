package harness.webUI.style

import harness.webUI.rawVDOM

sealed abstract class StyleElement
object StyleElement {
  final case class Attr(attr: rawVDOM.VDom.CSSAttr) extends StyleElement
  final case class Many(children: List[StyleElement]) extends StyleElement
  final case class InPseudoClass(pseudoClassName: String, children: List[StyleElement]) extends StyleElement
  final case class InTag(tagName: String, children: List[StyleElement]) extends StyleElement

  def apply(styleElements: StyleElement*): StyleElement.Many =
    StyleElement.Many(styleElements.toList)

}

def inTag(tagName: String)(attrs: StyleElement*): StyleElement.InTag = StyleElement.InTag(tagName, attrs.toList)
def inTag(tagName0: String, tagName1: String, tagNameN: String*)(attrs: StyleElement*): StyleElement =
  StyleElement.Many((tagName0 :: tagName1 :: tagNameN.toList).map(inTag(_)(attrs*)))
