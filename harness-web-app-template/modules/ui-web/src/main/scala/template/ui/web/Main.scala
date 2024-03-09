package template.ui.web

import harness.payments.*
import harness.webUI.*
import harness.webUI.style.*
import template.api.model as ApiModel
import zio.*

object Main extends PageApp[ApiModel.config.UiConfig] {

  override protected val preload: RIO[Environment, Unit] =
    ZIO.serviceWithZIO[ApiModel.config.UiConfig] { cfg => PaymentsUI.initStripe(cfg.stripePublishableKey) }

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
