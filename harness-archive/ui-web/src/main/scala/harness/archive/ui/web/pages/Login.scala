package harness.archive.ui.web.pages

import cats.data.EitherNel
import cats.syntax.either.*
import harness.archive.model as D
import harness.archive.ui.web.helpers.*
import harness.webUI.*
import harness.webUI.vdom.{given, *}
import harness.webUI.widgets.*
import harness.zio.*
import zio.json.*

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
              FormWidgets.labeled.requiredTextInput[String]("Username:", "username").zoomOut[D.user.Login](_.username) <*>
                FormWidgets.labeled.requiredTextInput[String]("Password:", "password", inputModifier = `type`.password).zoomOut[D.user.Login](_.password) <*>
                Widgets.stdSubmit("Login")
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
