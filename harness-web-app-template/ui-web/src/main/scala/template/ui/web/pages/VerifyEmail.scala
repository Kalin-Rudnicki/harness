package template.ui.web.pages

import _root_.template.model as D
import _root_.template.ui.web.helpers.*
import cats.data.EitherNel
import cats.syntax.either.*
import harness.webUI.*
import harness.webUI.style.{given, *}
import harness.webUI.vdom.{given, *}
import harness.webUI.widgets.*
import harness.zio.*
import zio.*

object VerifyEmail {

  final case class Env(
      user: D.user.User,
      code: String,
  )

  val page: Page =
    Page.builder
      .fetchState[Env] {
        for {
          user <- Api.user.fromSessionTokenOrRedirectToLogin(true)
        } yield Env(user, "")
      }
      .constTitle("Verify Email Address")
      .body {
        PModifier(
          Widgets.signedInNavBar.zoomOut[Env](_.user).zoomOutToPage,
          PageWidgets.pageBody(
            h1("Verify Email Address"),
            PModifier.builder.withState[Env] { env =>
              Option.when(env.user.emailIsVerified)(h2("You already verified your email!", color.red))
            },
            (
              FormWidgets
                .textInput[D.user.EmailVerificationCode]
                .labelRequired(
                  "Verification Code:",
                  "verification-code",
                  inputModifier = width := "40ch",
                )
                .zoomOut[Env](_.code) <*>
                FormWidgets.submitButton("Verify")
            )
              .flatMapActionVZ { (_, code) =>
                Api.user
                  .verifyEmail(code)
                  .as(Raise.History.push(Url("page", "home")()))
              },
            br,
            br,
            PModifier.builder.withRaise { rh =>
              button(DefaultStyleSheet.button.primary)(
                "Resend Code",
                onClick := { _ => rh.raiseManyZIO { Api.user.resendEmailCode.as(Nil) } },
              )
            },
          ),
        )
      }
      .logA

}
