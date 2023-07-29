package template.ui.web.pages

import _root_.template.model as D
import _root_.template.ui.web.helpers.*
import cats.syntax.either.*
import harness.webUI.*
import harness.webUI.vdom.{given, *}
import harness.webUI.widgets.*

object Home {

  final case class Env(
      user: D.user.User,
  )

  val page: Page =
    Page.builder
      .fetchState {
        for {
          user <- Api.user.fromSessionTokenOrRedirectToLogin
        } yield Env(user)
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
