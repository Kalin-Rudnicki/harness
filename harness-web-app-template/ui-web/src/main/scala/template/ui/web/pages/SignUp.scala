package template.ui.web.pages

import _root_.template.model as D
import _root_.template.ui.web.helpers.*
import cats.data.EitherNel
import cats.syntax.either.*
import harness.email.EmailAddress
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
              FormWidgets.textInput[String].labelRequired("First Name:", "first-name").zoomOut[Env](_.firstName) <*>
                FormWidgets.textInput[String].labelRequired("Last Name:", "last-name").zoomOut[Env](_.lastName) <*>
                FormWidgets.textInput[String].labelRequired("Username:", "username").zoomOut[Env](_.username) <*>
                (
                  FormWidgets.textInput[String].labelRequired("Password:", "password", inputModifier = `type`.password).zoomOut[Env.Passwords](_.password) <*>
                    FormWidgets.textInput[String].labelRequired("Confirm Password:", "confirm-password", inputModifier = `type`.password).zoomOut[Env.Passwords](_.confirmPassword)
                ).zoomOut[Env](_.passwords).flatMapValue(Env.Passwords(_, _).validate) <*>
                FormWidgets.textInput[EmailAddress].labelRequired("Email:", "email", inputModifier = `type`.email).zoomOut[Env](_.email) <*>
                FormWidgets.submitButton("Sign Up")
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
