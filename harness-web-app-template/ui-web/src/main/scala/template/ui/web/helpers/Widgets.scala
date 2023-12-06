package template.ui.web.helpers

import _root_.template.model as D
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

  private val logOutItem =
    NavBar.item(
      "Log Out",
      PModifier.builder.withRaise { rh =>
        onClick := { _ =>
          rh.raiseZIO(
            Api.user.logOut.as(Raise.History.push(Url("page", "login")())),
          )
        }
      },
    )

  val signedInNavBar: Modifier[D.user.User] =
    PModifier.builder.withRaise.withState[D.user.User] { (rh, s) =>
      if (s.emailIsVerified)
        NavBar(
          NavBar.linkItem(Url("page")(), "Index"),
        )(
          NavBar.linkItem(Url("page", "home")(), "Home"),
          NavBar.linkItem(Url("page", "account")(), s.firstName),
          logOutItem,
        )
      else
        NavBar(
          NavBar.linkItem(Url("page")(), "Index"),
        )(
          NavBar.linkItem(Url("page", "verify-email")(), "Verify Email"),
          logOutItem,
        )
    }

  val optNavBar: Modifier[Option[D.user.User]] =
    SumWidgets.option(signedInNavBar, signedOutNavBar).unit

}
