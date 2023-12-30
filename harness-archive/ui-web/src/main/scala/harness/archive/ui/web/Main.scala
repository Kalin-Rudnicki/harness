package harness.archive.ui.web

import harness.core.RunMode
import harness.webUI.*
import harness.webUI.style.*
import harness.zio.Logger

object Main extends PageApp {

  override protected val runMode: RunMode = RunMode.Prod

  override protected val logTolerance: Logger.LogLevel = Logger.LogLevel.Debug

  override val styleSheets: List[StyleSheet] = List(DefaultStyleSheet)

  override val routeMatcher: RouteMatcher.Root =
    "page" /: RouteMatcher.root(
      RouteMatcher.const { pages.Index.page },
      "home" /: RouteMatcher.const { pages.Home.page },
      "login" /: RouteMatcher.const { pages.Login.page },
      "sign-up" /: RouteMatcher.const { pages.SignUp.page },
      "logs" /: RouteMatcher.const { pages.Logs.page },
    )

}
