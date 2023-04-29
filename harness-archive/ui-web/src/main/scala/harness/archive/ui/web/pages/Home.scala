package harness.archive.ui.web.pages

import cats.syntax.either.*
import harness.archive.model as D
import harness.archive.ui.web.helpers.*
import harness.webUI.*
import harness.webUI.vdom.{given, *}
import harness.webUI.widgets.*

object Home {

  final case class Env(
      user: D.user.User,
  )

  val page: Page =
    Page.builder
      .fetchStateOrRedirect {
        Api.user.fromSessionTokenOptional.map {
          case Some(user) => Env(user).asRight
          case None       => Url("page", "login")().asLeft
        }
      }
      .constTitle("Home")
      .body {
        PModifier(
          Widgets.signedInNavBar.zoomOut[Env](_.user).zoomOutToPage,
          PageWidgets.pageBody(
            h1("Home"),
            PModifier.builder.withState[D.user.User] { u => p(s"Welcome, ${u.firstName}") }.zoomOut[Env](_.user),
          ),
        )
      }
      .logA

}
