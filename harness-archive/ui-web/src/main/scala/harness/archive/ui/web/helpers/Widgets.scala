package harness.archive.ui.web.helpers

import harness.archive.model as D
import harness.core.*
import harness.webUI.*
import harness.webUI.style.{given, *}
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
    SumWidgets.option(signedInNavBar, signedOutNavBar).unit


  val stdSubmit: CNodeWidgetA[Submit] =
    FormWidgets.formSubmitButton(
      DefaultStyleSheet.formSubmit,
    )

}
