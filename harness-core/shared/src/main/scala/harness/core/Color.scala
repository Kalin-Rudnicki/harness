package harness.core

sealed trait Color {
  def fgMod: String
  def bgMod: String
  final def fgANSI: String = s"$ANSIEscapeString${fgMod}m"
  final def bgANSI: String = s"$ANSIEscapeString${bgMod}m"
}

object Color {

  def apply(r: Int, g: Int, b: Int): Color =
    RGB(r, g, b)

  def apply(hex: Int): Color =
    RGB((hex >> 4) & 0xff, (hex >> 2) & 0xff, hex & 0xff)

  sealed trait Simple extends Color

  sealed abstract class Named(n: Char) extends Color.Simple {
    override def fgMod: String = s"3$n"
    override def bgMod: String = s"4$n"
  }
  object Named {
    case object Black extends Named('0')
    case object Red extends Named('1')
    case object Green extends Named('2')
    case object Yellow extends Named('3')
    case object Blue extends Named('4')
    case object Magenta extends Named('5')
    case object Cyan extends Named('6')
    case object White extends Named('7')
  }

  final case class RGB(r: Int, g: Int, b: Int) extends Color {
    override def fgMod: String = s"38;2;$r;$g;$b"
    override def bgMod: String = s"48;2;$r;$g;$b"
  }

  case object Default extends Color.Simple {
    override def fgMod: String = "39"
    override def bgMod: String = "49"
  }

}
