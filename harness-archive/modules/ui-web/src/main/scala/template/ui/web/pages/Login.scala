package template.ui.web.pages

import cats.data.EitherNel
import cats.syntax.either.*
import harness.webUI.*
import harness.webUI.vdom.{given, *}
import harness.webUI.widgets.*
import template.api.model as ApiModel
import template.ui.web.helpers.*

object Login {

  val page: Page =
    Page.builder
      .fetchState[ApiModel.user.Login] {
        for {
          _ <- Api.user.redirectToHomeIfLoggedIn
        } yield ApiModel.user.Login("", "")
      }
      .constTitle("Login")
      .body {
        PModifier(
          Widgets.signedOutNavBar.zoomOutToPage,
          PageWidgets.pageBody(
            h1("Login"),
            (
              FormWidgets.textInput[String].labelRequired("Username:", "username").zoomOut[ApiModel.user.Login](_.username) <*>
                FormWidgets.textInput[String].labelRequired("Password:", "password", inputModifier = `type`.password).zoomOut[ApiModel.user.Login](_.password) <*>
                FormWidgets.submitButton("Login")
            ).mapValue(ApiModel.user.Login.apply)
              .flatMapActionVZ { (_, login) =>
                Api.user
                  .login(login)
                  .toPageTask
                  .as(Raise.History.push(Url("page", "home")()))
              },
          ),
        )
      }
      .logA

}
