package harness.web.client.vdom

import harness.web.client.rawVDOM.VDom
import scala.scalajs.js

abstract class StdAttrBuilder(scopedName: VDom.ScopedName) {
  final def :=(value: String): CModifier = PModifier(VDom.StdAttr(scopedName, value))
}

object `type` extends StdAttrBuilder("type") { self =>
  inline def text: CModifier = self := "text"
  inline def number: CModifier = self := "number"
  inline def password: CModifier = self := "password"
  inline def checkbox: CModifier = self := "checkbox"
  inline def radio: CModifier = self := "radio"
  inline def submit: CModifier = self := "submit"
  inline def reset: CModifier = self := "reset"
  inline def button: CModifier = self := "button"
  inline def hidden: CModifier = self := "hidden"
  inline def image: CModifier = self := "image"
  inline def file: CModifier = self := "file"
  inline def color: CModifier = self := "color"
  inline def date: CModifier = self := "date"
  inline def datetime: CModifier = self := "datetime"
  inline def datetimeLocal: CModifier = self := "datetime-local"
  inline def email: CModifier = self := "email"
  inline def month: CModifier = self := "month"
  inline def range: CModifier = self := "range"
  inline def search: CModifier = self := "search"
  inline def tel: CModifier = self := "tel"
  inline def time: CModifier = self := "time"
  inline def url: CModifier = self := "url"
  inline def week: CModifier = self := "week"
}
