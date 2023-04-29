package harness.archive.ui.web.pages

import harness.archive.ui.web.helpers.*
import harness.webUI.*
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
          div(
            CssClass.b("page"),
            PageWidgets.pageMessages,
            div(
              CssClass.be("page", "body"),
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
                    // rh.raise(Raise.DisplayMessage(PageMessage.error(s"Hello world! [${java.time.LocalDateTime.now()}]")))
                    rh.raiseZIO(zio.ZIO.fail(harness.core.HError.UserError(s"Hello world! [${java.time.LocalDateTime.now()}]")))
                  },
                )
              },
            ),
          ),
        )
      }
      .logA

}
