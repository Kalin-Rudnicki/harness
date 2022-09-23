package harness.web.client.vdom

import harness.web.client.rawVDOM.VDom.ClassName

object CssClass {

  def b(block: String): CModifier = PModifier(ClassName.Block(block, Set.empty))
  def b(block: String, m0: String, mN: String*): CModifier = PModifier(ClassName.Block(block, (m0 :: mN.toList).toSet))
  def b(block: String, m0: IterableOnce[String], mN: IterableOnce[String]*): CModifier = PModifier(ClassName.Block(block, (m0 :: mN.toList).toSet.flatten))

  def be(block: String, element: String): CModifier = PModifier(ClassName.Element(block, element, Set.empty))
  def be(block: String, element: String, m0: String, mN: String*): CModifier = PModifier(ClassName.Element(block, element, (m0 :: mN.toList).toSet))
  def be(block: String, element: String, m0: IterableOnce[String], mN: IterableOnce[String]*): CModifier = PModifier(ClassName.Element(block, element, (m0 :: mN.toList).toSet.flatten))

}
