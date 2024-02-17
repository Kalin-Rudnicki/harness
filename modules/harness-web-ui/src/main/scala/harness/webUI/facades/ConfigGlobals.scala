package harness.webUI.facades

import scalajs.js

object ConfigGlobals {

  @js.native
  @js.annotation.JSGlobal("harnessUiConfig")
  val harnessUiConfig: String = js.native

}
