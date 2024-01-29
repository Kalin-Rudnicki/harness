package template.ui.web.pages

import _root_.template.model as D
import _root_.template.ui.web.helpers.*
import cats.data.EitherNel
import cats.syntax.either.*
import harness.webUI.*
import harness.webUI.vdom.{given, *}
import harness.webUI.widgets.*
import harness.zio.*

object Login {

  val page: Page =
    Page.builder
      .fetchState[D.user.Login] {
        for {
          _ <- Api.user.redirectToHomeIfLoggedIn
        } yield D.user.Login("", "")
      }
      .constTitle("Login")
      .body {
        PModifier(
          Widgets.signedOutNavBar.zoomOutToPage,
          PageWidgets.pageBody(
            h1("Login"),
            (
              FormWidgets.textInput[String].labelRequired("Username:", "username").zoomOut[D.user.Login](_.username) <*>
                FormWidgets.textInput[String].labelRequired("Password:", "password", inputModifier = `type`.password).zoomOut[D.user.Login](_.password) <*>
                FormWidgets.submitButton("Login")
            ).mapValue(D.user.Login.apply)
              .flatMapActionVZ { (_, login) =>
                Api.user
                  .login(login)
                  .as(Raise.History.push(Url("page", "home")()))
              },
          ),
        )
      }
      .logA

}
