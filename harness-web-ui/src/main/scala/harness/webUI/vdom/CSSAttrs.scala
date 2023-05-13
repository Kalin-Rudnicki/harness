package harness.webUI.vdom

import harness.webUI.rawVDOM
import org.scalajs.dom.MouseEvent
import scala.scalajs.js

abstract class CSSAttrBuilder(scopedName: rawVDOM.VDom.ScopedName) { self =>
  final def :=(value: String): CModifier = PModifier.cssAttr(scopedName, value)

  inline final def inherit: CModifier = self := "inherit"
  inline final def initial: CModifier = self := "initial"
  inline final def unset: CModifier = self := "unset"
  inline final def revert: CModifier = self := "revert"
}

abstract class ColorCSSAttrBuilder(scopedName: rawVDOM.VDom.ScopedName) extends CSSAttrBuilder(scopedName) { self =>
  inline final def red: CModifier = self := "red"
  inline final def blue: CModifier = self := "blue"
  inline final def green: CModifier = self := "green"
  inline final def black: CModifier = self := "black"
  inline final def white: CModifier = self := "white"
  inline final def gray: CModifier = self := "gray"
  inline final def yellow: CModifier = self := "yellow"
  inline final def orange: CModifier = self := "orange"
  inline final def purple: CModifier = self := "purple"
  inline final def brown: CModifier = self := "brown"
  inline final def pink: CModifier = self := "pink"
  inline final def cyan: CModifier = self := "cyan"
  inline final def magenta: CModifier = self := "magenta"
  inline final def transparent: CModifier = self := "transparent"
  inline final def silver: CModifier = self := "silver"
  inline final def lime: CModifier = self := "lime"
  inline final def maroon: CModifier = self := "maroon"
  inline final def olive: CModifier = self := "olive"
  inline final def teal: CModifier = self := "teal"
  inline final def navy: CModifier = self := "navy"
  inline final def fuchsia: CModifier = self := "fuchsia"
  inline final def aqua: CModifier = self := "aqua"
  inline final def rgb(r: Int, g: Int, b: Int): CModifier = self := s"rgb($r, $g, $b)"
}

object color extends ColorCSSAttrBuilder("color")
object backgroundColor extends ColorCSSAttrBuilder("background-color")
object width extends CSSAttrBuilder("width")
object height extends CSSAttrBuilder("height")
object cursor extends CSSAttrBuilder("cursor") { self =>
  inline def pointer: CModifier = self := "pointer"
  inline def auto: CModifier = self := "auto"
  inline def crosshair: CModifier = self := "crosshair"
  inline def default: CModifier = self := "default"
  inline def move: CModifier = self := "move"
  inline def text: CModifier = self := "text"
  inline def help: CModifier = self := "help"
  inline def progress: CModifier = self := "progress"
  inline def noDrop: CModifier = self := "noDrop"
  inline def notAllowed: CModifier = self := "notAllowed"
  inline def eResize: CModifier = self := "eResize"
  inline def nResize: CModifier = self := "nResize"
  inline def neResize: CModifier = self := "neResize"
  inline def nwResize: CModifier = self := "nwResize"
  inline def sResize: CModifier = self := "sResize"
  inline def seResize: CModifier = self := "seResize"
  inline def swResize: CModifier = self := "swResize"
  inline def wResize: CModifier = self := "wResize"
  inline def ewResize: CModifier = self := "ewResize"
  inline def nsResize: CModifier = self := "nsResize"
  inline def neswResize: CModifier = self := "neswResize"
  inline def nwseResize: CModifier = self := "nwseResize"
  inline def colResize: CModifier = self := "colResize"
  inline def rowResize: CModifier = self := "rowResize"
  inline def allScroll: CModifier = self := "allScroll"
  inline def zoomIn: CModifier = self := "zoomIn"
  inline def zoomOut: CModifier = self := "zoomOut"
}
object userSelect extends CSSAttrBuilder("user-select") { self =>
  inline def none: CModifier = self := "none"
  inline def text: CModifier = self := "text"
  inline def all: CModifier = self := "all"
  inline def auto: CModifier = self := "auto"
  inline def contain: CModifier = self := "contain"
  inline def allScroll: CModifier = self := "allScroll"
  inline def element: CModifier = self := "element"
  inline def elements: CModifier = self := "elements"
  inline def toggle: CModifier = self := "toggle"
}
object padding extends CSSAttrBuilder("padding")
object paddingTop extends CSSAttrBuilder("padding-top")
object paddingRight extends CSSAttrBuilder("padding-right")
object paddingBottom extends CSSAttrBuilder("padding-bottom")
object paddingLeft extends CSSAttrBuilder("padding-left")
object margin extends CSSAttrBuilder("margin")
object marginTop extends CSSAttrBuilder("margin-top")
object marginRight extends CSSAttrBuilder("margin-right")
object marginBottom extends CSSAttrBuilder("margin-bottom")
object marginLeft extends CSSAttrBuilder("margin-left")
object display extends CSSAttrBuilder("display") { self =>
  inline def inline: CModifier = self := "inline"
  inline def block: CModifier = self := "block"
  inline def inlineBlock: CModifier = self := "inline-block"
  inline def flex: CModifier = self := "flex"
  inline def inlineFlex: CModifier = self := "inline-flex"
  inline def table: CModifier = self := "table"
  inline def tableCell: CModifier = self := "table-cell"
  inline def tableRow: CModifier = self := "table-row"
  inline def tableRowGroup: CModifier = self := "table-row-group"
  inline def tableHeaderGroup: CModifier = self := "table-header-group"
  inline def tableFooterGroup: CModifier = self := "table-footer-group"
  inline def tableColumn: CModifier = self := "table-column"
  inline def tableColumnGroup: CModifier = self := "table-column-group"
  inline def tableCaption: CModifier = self := "table-caption"
  inline def none: CModifier = self := "none"
}
object textAlign extends CSSAttrBuilder("text-align")
object verticalAlign extends CSSAttrBuilder("vertical-align")
object fontSize extends CSSAttrBuilder("font-size")
object fontWeight extends CSSAttrBuilder("font-weight")
object fontFamily extends CSSAttrBuilder("font-family")
object borderRadius extends CSSAttrBuilder("border-radius")
object borderWidth extends CSSAttrBuilder("border-width")
object borderStyle extends CSSAttrBuilder("border-style")
object borderColor extends ColorCSSAttrBuilder("border-color")
object borderTop extends CSSAttrBuilder("border-top")
object borderRight extends CSSAttrBuilder("border-right")
object borderBottom extends CSSAttrBuilder("border-bottom")
object borderLeft extends CSSAttrBuilder("border-left")
object border extends CSSAttrBuilder("border")
object whiteSpace extends CSSAttrBuilder("white-space") { self =>
  inline def normal: CModifier = self := "normal"
  inline def noWrap: CModifier = self := "nowrap"
  inline def pre: CModifier = self := "pre"
  inline def preWrap: CModifier = self := "pre-wrap"
  inline def preLine: CModifier = self := "pre-line"
  inline def breakSpaces: CModifier = self := "break-spaces"
}
object boxShadow extends CSSAttrBuilder("box-shadow")
object textShadow extends CSSAttrBuilder("text-shadow")
object transform extends CSSAttrBuilder("transform")
object opacity extends CSSAttrBuilder("opacity")
object visibility extends CSSAttrBuilder("visibility")
object zIndex extends CSSAttrBuilder("z-index")
object overflow extends CSSAttrBuilder("overflow")
object overflowX extends CSSAttrBuilder("overflow-x")
object overflowY extends CSSAttrBuilder("overflow-y")
object transformOrigin extends CSSAttrBuilder("transform-origin")
object animationName extends CSSAttrBuilder("animation-name")
object animationDuration extends CSSAttrBuilder("animation-duration")
object animationTimingFunction extends CSSAttrBuilder("animation-timing-function")
object animationDelay extends CSSAttrBuilder("animation-delay")
object animationDirection extends CSSAttrBuilder("animation-direction")
object animationIterationCount extends CSSAttrBuilder("animation-iteration-count")
object animationFillMode extends CSSAttrBuilder("animation-fill-mode")
object animationPlayState extends CSSAttrBuilder("animation-play-state")
