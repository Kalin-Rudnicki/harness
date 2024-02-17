package harness.webUI.style

sealed abstract class PseudoClass(val name: String) {
  def apply(attrs: StyleElement*): StyleElement.InPseudoClass = StyleElement.InPseudoClass(name, attrs.toList)
}
object PseudoClass {
  case object Active extends PseudoClass("active")
  case object Focus extends PseudoClass("focus")
  case object Hover extends PseudoClass("hover")
  case object Visited extends PseudoClass("visited")
  case object Link extends PseudoClass("link")
  case object Target extends PseudoClass("target")
  case object FirstChild extends PseudoClass("first-child")
  case object LastChild extends PseudoClass("last-child")
  // TODO (KR) : NthChild
}
