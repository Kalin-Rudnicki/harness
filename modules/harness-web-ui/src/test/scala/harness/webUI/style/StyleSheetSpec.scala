package harness.webUI.style

import harness.test.*
import harness.webUI.style.ColorPalate.implicits.default
import zio.test.*

object StyleSheetSpec extends PlainHarnessSpec {

  override def spec: TestSpec =
    suite("StyleSheetSpec")(
      test("show toString") {
        val ss = new DefaultStyleSheet {}
        val str = ss.toString
        println(s"```css\n$str\n```")
        assertCompletes
      },
    )

}
