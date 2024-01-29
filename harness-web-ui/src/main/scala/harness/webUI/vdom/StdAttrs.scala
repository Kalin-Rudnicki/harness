package harness.webUI.vdom

import harness.webUI.rawVDOM.VDom

abstract class StdAttrBuilder[T](scopedName: VDom.ScopedName, convert: T => String = (_: T).toString) { self =>
  final def :=(value: T): CModifier = PModifier.stdAttr(scopedName, convert(value))

  inline final def empty(implicit ev: String <:< T): CModifier = self := ev("")
}

object `type` extends StdAttrBuilder[String]("type") { self =>
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

object `for` extends StdAttrBuilder[String]("for")
object id extends StdAttrBuilder[String]("id")
object src extends StdAttrBuilder[String]("src")

object rowSpan extends StdAttrBuilder[Int]("rowSpan")
object colSpan extends StdAttrBuilder[Int]("colSpan")

object multiple extends StdAttrBuilder[String]("multiple")
object controls extends StdAttrBuilder[String]("controls")
object autoplay extends StdAttrBuilder[String]("autoplay")
