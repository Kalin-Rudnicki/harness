package harness.archive.ui.web.helpers

import harness.archive.model as D
import harness.core.*
import harness.webUI.*
import harness.webUI.vdom.{given, *}
import harness.webUI.widgets.*

object Widgets {

  val signedOutNavBar: CModifier =
    NavBar(
      NavBar.linkItem(Url("page")(), "Index"),
    )(
      NavBar.linkItem(Url("page", "sign-up")(), "Sign Up"),
      NavBar.linkItem(Url("page", "login")(), "Login"),
    )

  val signedInNavBar: Modifier[D.user.User] =
    NavBar(
      NavBar.linkItem(Url("page")(), "Index"),
    )(
      NavBar.linkItem(Url("page", "home")(), "Home"),
      PModifier.builder.withState[D.user.User] { s =>
        NavBar.linkItem(Url("page", "account")(), s.firstName)
      },
      NavBar.item(
        "Log Out",
        PModifier.builder.withRaise { rh =>
          onClick := { _ =>
            rh.raiseZIO(
              Api.user.logOut.as(Raise.History.push(Url("page", "login")())),
            )
          }
        },
      ),
    )

  val optNavBar: Modifier[Option[D.user.User]] =
    SumWidget.option(signedInNavBar, signedOutNavBar).unit

  def stdInput[V: StringDecoder](
      _label: String,
      _id: String,
      inputModifier: CModifier = PModifier(),
      labelModifier: CModifier = PModifier(),
  ): ModifierAV[Submit, String, V] =
    div(CssClass.b("form-field")).defer(
      formInput[V]
        .apply(
          CssClass.be("form-field", "input"),
          id := _id,
          inputModifier,
        )
        .required
        .labeled(
          _label,
          label(
            _,
            CssClass.be("form-field", "label"),
            `for` := _id,
            labelModifier,
          ),
        ),
    )

  val stdSubmit: CNodeWidgetA[Submit] =
    formSubmitButton(
      CssClass.b("form-submit"),
    )

}
