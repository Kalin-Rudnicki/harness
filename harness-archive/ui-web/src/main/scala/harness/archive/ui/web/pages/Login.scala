package harness.archive.ui.web.pages

import harness.archive.model as D
import harness.archive.ui.web.helpers.*
import cats.data.EitherNel
import cats.syntax.either.*
import harness.webUI.*
import harness.webUI.vdom.{given, *}
import harness.webUI.widgets.*
import harness.zio.*
import zio.json.*

object Login {

  val page: Page =
    Page.builder
      .fetchStateOrRedirect[D.user.Login] {
        Api.user.fromSessionTokenOptional.map {
          case Some(_) => Url("page", "home")().asLeft
          case None    => D.user.Login("", "").asRight
        }
      }
      .constTitle("Login")
      .body {
        PModifier(
          Widgets.signedOutNavBar.zoomOutToPage,
          PageWidgets.pageBody(
            h1("Login"),
            (
              Widgets.stdInput[String]("Username:", "username").zoomOut[D.user.Login](_.username) <*>
                Widgets.stdInput[String]("Password:", "password", inputModifier = `type`.password).zoomOut[D.user.Login](_.password) <*>
                Widgets.stdSubmit("Login")
            ).mapValue(D.user.Login.apply)
              .mapActionV { (login, _) =>
                Api.user
                  .login(login)
                  .as(Raise.History.push(Url("page", "home")()) :: Nil)
              },
          ),
        )
      }
      .logA

}