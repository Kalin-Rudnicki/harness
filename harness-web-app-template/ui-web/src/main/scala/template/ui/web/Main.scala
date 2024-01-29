package template.ui.web

import harness.core.RunMode
import harness.payments.*
import harness.web.HasStdClientConfig
import harness.webUI.*
import harness.webUI.style.*
import harness.zio.*
import template.model as D
import zio.*

object Main extends PageApp[D.config.UiConfig] {

  override protected val preload: SHRIO[D.config.UiConfig, Unit] =
    ZIO.serviceWithZIO[D.config.UiConfig] { cfg => PaymentsUI.initStripe(cfg.stripePublishableKey) }

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
