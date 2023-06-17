package harness.webUI.vdom

implicit class CssDoubleOps(self: Double) {
  def rem: String = s"${self}rem"
  def em: String = s"${self}em"
  def pt: String = s"${self}pt"
  def pct: String = s"$self%"
}

implicit class CssIntOps(self: Int) {
  def px: String = s"${self}px"
  def ch: String = s"${self}ch"
}
