package harness.webUI.vdom

import harness.webUI.rawVDOM
import harness.webUI.rawVDOM.VDom.CSSAttr

abstract class CSSAttrBuilder(scopedName: rawVDOM.VDom.ScopedName) { self =>
  final def :=(value0: String): CSSAttr = CSSAttr(scopedName, value0)

  inline final def inherit: CSSAttr = self := "inherit"
  inline final def initial: CSSAttr = self := "initial"
  inline final def unset: CSSAttr = self := "unset"
  inline final def revert: CSSAttr = self := "revert"
}

abstract class ColorCSSAttrBuilder(scopedName: rawVDOM.VDom.ScopedName) extends CSSAttrBuilder(scopedName) { self =>
  inline final def red: CSSAttr = self := "red"
  inline final def blue: CSSAttr = self := "blue"
  inline final def green: CSSAttr = self := "green"
  inline final def black: CSSAttr = self := "black"
  inline final def white: CSSAttr = self := "white"
  inline final def gray: CSSAttr = self := "gray"
  inline final def yellow: CSSAttr = self := "yellow"
  inline final def orange: CSSAttr = self := "orange"
  inline final def purple: CSSAttr = self := "purple"
  inline final def brown: CSSAttr = self := "brown"
  inline final def pink: CSSAttr = self := "pink"
  inline final def cyan: CSSAttr = self := "cyan"
  inline final def magenta: CSSAttr = self := "magenta"
  inline final def transparent: CSSAttr = self := "transparent"
  inline final def silver: CSSAttr = self := "silver"
  inline final def lime: CSSAttr = self := "lime"
  inline final def maroon: CSSAttr = self := "maroon"
  inline final def olive: CSSAttr = self := "olive"
  inline final def teal: CSSAttr = self := "teal"
  inline final def navy: CSSAttr = self := "navy"
  inline final def fuchsia: CSSAttr = self := "fuchsia"
  inline final def aqua: CSSAttr = self := "aqua"
  inline final def rgb(r: Int, g: Int, b: Int): CSSAttr = self := s"rgb($r, $g, $b)"
}

object color extends ColorCSSAttrBuilder("color")
object backgroundColor extends ColorCSSAttrBuilder("background-color")
object width extends CSSAttrBuilder("width")
object minWidth extends CSSAttrBuilder("min-width")
object maxWidth extends CSSAttrBuilder("max-width")
object height extends CSSAttrBuilder("height")
object minHeight extends CSSAttrBuilder("min-height")
object maxHeight extends CSSAttrBuilder("max-height")
object cursor extends CSSAttrBuilder("cursor") { self =>
  inline def pointer: CSSAttr = self := "pointer"
  inline def auto: CSSAttr = self := "auto"
  inline def crosshair: CSSAttr = self := "crosshair"
  inline def default: CSSAttr = self := "default"
  inline def move: CSSAttr = self := "move"
  inline def text: CSSAttr = self := "text"
  inline def help: CSSAttr = self := "help"
  inline def progress: CSSAttr = self := "progress"
  inline def noDrop: CSSAttr = self := "noDrop"
  inline def notAllowed: CSSAttr = self := "notAllowed"
  inline def eResize: CSSAttr = self := "eResize"
  inline def nResize: CSSAttr = self := "nResize"
  inline def neResize: CSSAttr = self := "neResize"
  inline def nwResize: CSSAttr = self := "nwResize"
  inline def sResize: CSSAttr = self := "sResize"
  inline def seResize: CSSAttr = self := "seResize"
  inline def swResize: CSSAttr = self := "swResize"
  inline def wResize: CSSAttr = self := "wResize"
  inline def ewResize: CSSAttr = self := "ewResize"
  inline def nsResize: CSSAttr = self := "nsResize"
  inline def neswResize: CSSAttr = self := "neswResize"
  inline def nwseResize: CSSAttr = self := "nwseResize"
  inline def colResize: CSSAttr = self := "colResize"
  inline def rowResize: CSSAttr = self := "rowResize"
  inline def allScroll: CSSAttr = self := "allScroll"
  inline def zoomIn: CSSAttr = self := "zoomIn"
  inline def zoomOut: CSSAttr = self := "zoomOut"
}
object userSelect extends CSSAttrBuilder("user-select") { self =>
  inline def none: CSSAttr = self := "none"
  inline def text: CSSAttr = self := "text"
  inline def all: CSSAttr = self := "all"
  inline def auto: CSSAttr = self := "auto"
  inline def contain: CSSAttr = self := "contain"
  inline def allScroll: CSSAttr = self := "allScroll"
  inline def element: CSSAttr = self := "element"
  inline def elements: CSSAttr = self := "elements"
  inline def toggle: CSSAttr = self := "toggle"
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
  inline def inline: CSSAttr = self := "inline"
  inline def block: CSSAttr = self := "block"
  inline def inlineBlock: CSSAttr = self := "inline-block"
  inline def flex: CSSAttr = self := "flex"
  inline def inlineFlex: CSSAttr = self := "inline-flex"
  inline def table: CSSAttr = self := "table"
  inline def tableCell: CSSAttr = self := "table-cell"
  inline def tableRow: CSSAttr = self := "table-row"
  inline def tableRowGroup: CSSAttr = self := "table-row-group"
  inline def tableHeaderGroup: CSSAttr = self := "table-header-group"
  inline def tableFooterGroup: CSSAttr = self := "table-footer-group"
  inline def tableColumn: CSSAttr = self := "table-column"
  inline def tableColumnGroup: CSSAttr = self := "table-column-group"
  inline def tableCaption: CSSAttr = self := "table-caption"
  inline def none: CSSAttr = self := "none"
}
object textAlign extends CSSAttrBuilder("text-align") { self =>
  inline def center: CSSAttr = self := "center"
}
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
  inline def normal: CSSAttr = self := "normal"
  inline def noWrap: CSSAttr = self := "nowrap"
  inline def pre: CSSAttr = self := "pre"
  inline def preWrap: CSSAttr = self := "pre-wrap"
  inline def preLine: CSSAttr = self := "pre-line"
  inline def breakSpaces: CSSAttr = self := "break-spaces"
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
object flex extends CSSAttrBuilder("flex")
object flexWrap extends CSSAttrBuilder("flex-wrap") { self =>
  inline def nowrap: CSSAttr = self := "nowrap"
  inline def wrap: CSSAttr = self := "wrap"
  inline def wrapReverse: CSSAttr = self := "wrap-reverse"
}
object boxSizing extends CSSAttrBuilder("box-sizing") { self =>
  inline def contentBox: CSSAttr = self := "content-box"
  inline def borderBox: CSSAttr = self := "border-box"
}
object justifyContent extends CSSAttrBuilder("justify-content") { self =>
  inline def flexStart: CSSAttr = self := "flex-start"
  inline def flexEnd: CSSAttr = self := "flex-end"
  inline def center: CSSAttr = self := "center"
  inline def spaceBetween: CSSAttr = self := "space-between"
  inline def spaceAround: CSSAttr = self := "space-around"
  inline def spaceEvenly: CSSAttr = self := "space-evenly"
}
object alignItems extends CSSAttrBuilder("align-items") { self =>
  inline def flexStart: CSSAttr = self := "flex-start"
  inline def flexEnd: CSSAttr = self := "flex-end"
  inline def center: CSSAttr = self := "center"
  inline def baseline: CSSAttr = self := "baseline"
  inline def stretch: CSSAttr = self := "stretch"
}
object outline extends CSSAttrBuilder("outline")
object borderCollapse extends CSSAttrBuilder("border-collapse") { self =>
  inline def collapse: CSSAttr = self := "collapse"
  inline def separate: CSSAttr = self := "separate"
}
