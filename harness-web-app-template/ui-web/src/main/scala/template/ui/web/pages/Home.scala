package template.ui.web.pages

import _root_.template.model as D
import _root_.template.ui.web.helpers.*
import cats.syntax.either.*
import harness.web.client.*
import harness.web.client.vdom.{given, *}

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
          Widgets.signedInNavBar.zoomOut[Env](_.user),
          div(
            CssClass.b("page"),
            h1("Home"),
            PModifier.builder.withState[D.user.User] { u => p(s"Welcome, ${u.firstName}") }.zoomOut[Env](_.user),
          ),
        )
      }
      .logA

}
