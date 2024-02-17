package harness.webUI.style

import harness.webUI.rawVDOM
import harness.webUI.vdom.{given, *}

given Conversion[rawVDOM.VDom.CSSAttr, StyleElement] = attr => StyleElement.Attr(attr)

given convertStyleSheetPart: Conversion[StyleSheet.StyleSheetPart, CModifier] = _.classNames
given convertOptStyleSheetPart: Conversion[Option[StyleSheet.StyleSheetPart], CModifier] = _.fold(PModifier())(_.classNames)
