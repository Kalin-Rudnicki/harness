package template.ui.web.pages

import _root_.template.ui.web.helpers.*
import harness.web.client.*
import harness.web.client.vdom.{given, *}

object Index {

  val page: Page =
    Page.builder
      .fetchState { Api.user.fromSessionTokenOptional }
      .constTitle("Index")
      .body {
        PModifier(
          Widgets.optNavBar,
          div(
            CssClass.b("page"),
            h1("Template"),
            p("Hopefully, an easy startup..."),
          ),
        )
      }
      .logA

}
