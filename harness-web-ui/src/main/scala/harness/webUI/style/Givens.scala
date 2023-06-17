package harness.webUI.style

import harness.webUI.rawVDOM
import harness.webUI.vdom.{given, *}

given Conversion[rawVDOM.VDom.CSSAttr, StyleElement] = attr => StyleElement.Attr(attr)

given convertBlock: Conversion[StyleSheet#Block, CModifier] = _.classNames
given convertBlockModifier: Conversion[StyleSheet#Block#Modifier, CModifier] = _.classNames
given convertBlockElement: Conversion[StyleSheet#Block#Element, CModifier] = _.classNames
given convertBlockElementModifier: Conversion[StyleSheet#Block#Element#Modifier, CModifier] = _.classNames
