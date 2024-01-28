package template.ui.web

import harness.core.RunMode
import harness.payments.*
import harness.webUI.*
import harness.webUI.style.*
import harness.zio.*

object Main extends PageApp {

  override protected val runMode: RunMode = RunMode.Dev
  override protected val logTolerance: Logger.LogLevel = Logger.LogLevel.Trace

  override protected val preload: SHTask[Unit] =
    PaymentsUI.addStripeSrc

  override val styleSheets: List[StyleSheet] = List(DefaultStyleSheet)

  override val routeMatcher: RouteMatcher.Root =
    "page" /: RouteMatcher.root(
      RouteMatcher.const { pages.Index.page },
      "home" /: RouteMatcher.const { pages.Home.page },
      "account" /: RouteMatcher.const { pages.Account.page },
      "login" /: RouteMatcher.const { pages.Login.page },
      "sign-up" /: RouteMatcher.const { pages.SignUp.page },
      "verify-email" /: RouteMatcher.const { pages.VerifyEmail.page },
    )

}
