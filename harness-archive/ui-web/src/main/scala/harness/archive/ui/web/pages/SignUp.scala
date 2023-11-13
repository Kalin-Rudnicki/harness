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
      .fetchState[Env] {
        for {
          _ <- Api.user.redirectToHomeIfLoggedIn
        } yield Env("", "", "", Env.Passwords("", ""), "")
      }
      .constTitle("Sign Up")
      .body {
        PModifier(
          Widgets.signedOutNavBar.zoomOutToPage,
          PageWidgets.pageBody(
            h1("Sign Up"),
            (
              FormWidgets.labeled.requiredTextInput[String]("First Name:", "first-name").zoomOut[Env](_.firstName) <*>
                FormWidgets.labeled.requiredTextInput[String]("Last Name:", "last-name").zoomOut[Env](_.lastName) <*>
                FormWidgets.labeled.requiredTextInput[String]("Username:", "username").zoomOut[Env](_.username) <*>
                (
                  FormWidgets.labeled.requiredTextInput[String]("Password:", "password", inputModifier = `type`.password).zoomOut[Env.Passwords](_.password) <*>
                    FormWidgets.labeled.requiredTextInput[String]("Confirm Password:", "confirm-password", inputModifier = `type`.password).zoomOut[Env.Passwords](_.confirmPassword)
                ).zoomOut[Env](_.passwords).flatMapValue(Env.Passwords(_, _).validate) <*>
                FormWidgets.labeled.requiredTextInput[String]("Email:", "email", inputModifier = `type`.email).zoomOut[Env](_.email) <*>
                Widgets.stdSubmit("Sign Up")
            ).mapValue(D.user.SignUp.apply)
              .flatMapActionVZ { (_, signUp) =>
                Api.user
                  .signUp(signUp)
                  .as(Raise.History.push(Url("page", "home")()))
              },
          ),
        )
      }
      .logA

}
