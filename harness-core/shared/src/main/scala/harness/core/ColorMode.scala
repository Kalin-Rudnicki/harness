package harness.core

import cats.data.NonEmptyList

sealed trait ColorMode {
  def selectColor(extended: Color, simple: Color.Simple): Color
  protected def ansiPrefix: String
  protected def ansiSuffix: String
  def fgMod(color: Color): ColorMode.ANSIMod
  def bgMod(color: Color): ColorMode.ANSIMod
  def ansiEscape(modifiers: NonEmptyList[ColorMode.ANSIMod]): String = modifiers.toList.mkString(ansiPrefix, ";", ansiSuffix)
  inline final def ansiEscape(modifier0: ColorMode.ANSIMod, modifierN: ColorMode.ANSIMod*): String = ansiEscape(NonEmptyList(modifier0, modifierN.toList))

  inline final def fgANSI(color: Color): String = ansiEscape(fgMod(color))
  inline final def fgANSI(extended: Color, simple: Color.Simple): String = fgANSI(selectColor(extended, simple))
  inline final def bgANSI(color: Color): String = ansiEscape(bgMod(color))
  inline final def bgANSI(extended: Color, simple: Color.Simple): String = bgANSI(selectColor(extended, simple))

  final def fgColorize(color: Color, str: Any): String =
    color match {
      case Color.Default => str.toString
      case _             => s"${fgANSI(color)}$str${fgANSI(Color.Default)}"
    }
  final def bgColorize(color: Color, str: Any): String =
    color match {
      case Color.Default => str.toString
      case _             => s"${bgANSI(color)}$str${bgANSI(Color.Default)}"
    }

}
object ColorMode {

  opaque type ANSIMod = String

  sealed trait Standard extends ColorMode {
    override protected final def ansiPrefix: String = ANSIEscapeString
    override protected final def ansiSuffix: String = "m"
    override final def fgMod(color: Color): ANSIMod = color.fgMod
    override final def bgMod(color: Color): ANSIMod = color.bgMod
  }

  case object Extended extends Standard {
    override def selectColor(extended: Color, simple: Color.Simple): Color = extended
  }
  case object Simple extends Standard {
    override def selectColor(extended: Color, simple: Color.Simple): Color = simple
  }

  case object Colorless extends ColorMode {
    override def selectColor(extended: Color, simple: Color.Simple): Color = Color.Default
    override protected def ansiPrefix: ANSIMod = ""
    override protected def ansiSuffix: ANSIMod = ""
    override def fgMod(color: Color): ANSIMod = ""
    override def bgMod(color: Color): ANSIMod = ""
    override def ansiEscape(modifiers: NonEmptyList[ANSIMod]): ANSIMod = ""
  }

  case object Show extends ColorMode {
    override def selectColor(extended: Color, simple: Color.Simple): Color = extended
    override protected def ansiPrefix: String = "[["
    override protected def ansiSuffix: String = "]]"
    override def fgMod(color: Color): ANSIMod =
      color match {
        case Color.RGB(r, g, b) => s"rgb($r, $g, $b)"
        case _                  => color.toString
      }
    override def bgMod(color: Color): ANSIMod =
      color match {
        case Color.RGB(r, g, b) => s"rgbBG($r, $g, $b)"
        case _                  => s"${color}BG"
      }
  }

  val all: Seq[ColorMode] =
    Seq(
      ColorMode.Extended,
      ColorMode.Simple,
      ColorMode.Colorless,
      ColorMode.Show,
    )

  val nameMap: Map[String, ColorMode] =
    all.map(cm => (cm.toString.toUpperCase, cm)).toMap

  implicit val stringEncoder: StringEncoder[ColorMode] =
    StringEncoder.usingToString

  implicit val stringDecoder: StringDecoder[ColorMode] =
    StringDecoder.fromOptionF("ColorMode", str => nameMap.get(str.toUpperCase))

}
