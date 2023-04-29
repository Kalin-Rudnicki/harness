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

object SignUp {

  final case class Env(
      firstName: String,
      lastName: String,
      username: String,
      passwords: Env.Passwords,
      email: String,
  )
  object Env {

    final case class Passwords(
        password: String,
        confirmPassword: String,
    ) {
      def validate: EitherNel[String, String] =
        if (password == confirmPassword) password.asRight
        else "Passwords do not match".leftNel
    }
    object Passwords {
      implicit val codec: JsonCodec[Env.Passwords] = DeriveJsonCodec.gen
    }

    implicit val codec: JsonCodec[Env] = DeriveJsonCodec.gen

  }

  val page: Page =
    Page.builder
      .fetchStateOrRedirect[Env] {
        Api.user.fromSessionTokenOptional.map {
          case Some(_) => Url("page", "home")().asLeft
          case None    => Env("", "", "", Env.Passwords("", ""), "").asRight
        }
      }
      .constTitle("Sign Up")
      .body {
        PModifier(
          Widgets.signedOutNavBar.zoomOutToPage,
          PageWidgets.pageBody(
            h1("Sign Up"),
            (
              Widgets.stdInput[String]("First Name:", "first-name").zoomOut[Env](_.firstName) <*>
                Widgets.stdInput[String]("Last Name:", "last-name").zoomOut[Env](_.lastName) <*>
                Widgets.stdInput[String]("Username:", "username").zoomOut[Env](_.username) <*>
                (
                  Widgets.stdInput[String]("Password:", "password", inputModifier = `type`.password).zoomOut[Env.Passwords](_.password) <*>
                    Widgets.stdInput[String]("Confirm Password:", "confirm-password", inputModifier = `type`.password).zoomOut[Env.Passwords](_.confirmPassword)
                ).zoomOut[Env](_.passwords).flatMapValue(Env.Passwords(_, _).validate) <*>
                Widgets.stdInput[String]("Email:", "email", inputModifier = `type`.email).zoomOut[Env](_.email) <*>
                Widgets.stdSubmit("Sign Up")
            ).mapValue(D.user.SignUp.apply)
              .mapActionV { (signUp, _) =>
                Api.user
                  .signUp(signUp)
                  .as(Raise.History.push(Url("page", "home")()) :: Nil)
              },
          ),
        )
      }
      .logA

}
