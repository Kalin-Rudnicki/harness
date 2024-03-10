package template.ui.web.pages

import harness.webUI.*
import harness.webUI.style.{given, *}
import harness.webUI.vdom.{given, *}
import harness.webUI.widgets.*
import template.ui.web.helpers.*

object Index {

  val page: Page =
    Page.builder
      .fetchState { Api.user.fromSessionTokenOptional.toPageLoadTask }
      .constTitle("Index")
      .body {
        PModifier(
          Widgets.optNavBar.zoomOutToPage,
          PageWidgets.pageBody(
            DefaultStyleSheet.page.body,
            h1("Template"),
            p("Hopefully, an easy startup..."),
            PModifier.builder.withRaise { rh =>
              button(
                "Message",
                onClick := { _ =>
                  rh.raise(Raise.DisplayMessage(PageMessage.info(s"Hello world! [${java.time.LocalDateTime.now()}]")))
                },
                onContextMenu := { e =>
                  e.preventDefault()
                  rh.raiseZIO(zio.ZIO.fail(harness.webUI.error.UIError.Failure.error(s"Hello world! [${java.time.LocalDateTime.now()}]")))
                },
              )
            },
          ),
        )
      }
      .logA

}
