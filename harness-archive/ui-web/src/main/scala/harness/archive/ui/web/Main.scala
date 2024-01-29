package harness.archive.ui.web

import harness.web.HasStdClientConfig
import harness.webUI.*
import harness.webUI.style.*

object Main extends PageApp[HasStdClientConfig.Basic] {

  override val styleSheets: List[StyleSheet] = List(DefaultStyleSheet)

  override val routeMatcher: RouteMatcher.Root =
    "page" /: RouteMatcher.root(
      RouteMatcher.const { pages.Index.page },
      "home" /: RouteMatcher.const { pages.Home.page },
      "login" /: RouteMatcher.const { pages.Login.page },
      "sign-up" /: RouteMatcher.const { pages.SignUp.page },
      "logs" /: RouteMatcher.ParamArg[String]("query") ??*:
        RouteMatcher.finish[Option[String]] { pages.Logs.page },
      "traces" /: RouteMatcher.ParamArg[String]("query") ??*:
        RouteMatcher.finish[Option[String]] { pages.Traces.page },
    )

}
