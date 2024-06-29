package harness.webUI.style

import harness.webUI.style.ColorPalate.implicits.default
import harness.zio.test.*
import zio.test.*

object StyleSheetSpec extends DefaultHarnessSpec {

  override def testSpec: TestSpec =
    suite("StyleSheetSpec")(
      test("show toString") {
        val ss = new DefaultStyleSheet {}
        val str = ss.toString
        println(s"```css\n$str\n```")
        assertCompletes
      },
    )

}
