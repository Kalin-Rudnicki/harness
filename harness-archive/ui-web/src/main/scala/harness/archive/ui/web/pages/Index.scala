package harness.archive.ui.web.pages

import harness.archive.ui.web.helpers.*
import harness.webUI.*
import harness.webUI.style.{given, *}
import harness.webUI.vdom.{given, *}
import harness.webUI.widgets.*

object Index {

  val page: Page =
    Page.builder
      .fetchState { Api.user.fromSessionTokenOptional }
      .constTitle("Index")
      .body {
        PModifier(
          Widgets.optNavBar.zoomOutToPage,
          PageWidgets.pageBody(
            h1("Harness Archive"),
          ),
        )
      }
      .logA

}
