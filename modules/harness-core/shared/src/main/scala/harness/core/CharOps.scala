package harness.core

implicit class HarnessCharOps(self: Char) {

  def unesc: String = self.unesc("'")
  def unesc(leftAndRight: String): String = self.unesc(leftAndRight, leftAndRight)
  def unesc(left: String, right: String): String = {
    val charText =
      self match {
        case '\n' => "\\n"
        case '\\' => "\\\\"
        case '\t' => "\\t"
        case '"'  => "\\\""
        case c    => c.toString
      }
    s"$left$charText$right"
  }

}
