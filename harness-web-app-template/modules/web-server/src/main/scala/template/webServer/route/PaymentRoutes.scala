package template.webServer.route

import harness.http.server.{given, *}
import harness.payments.model as PM
import harness.sql.query.Transaction
import harness.web.*
import template.domain.model.DomainError
import template.webServer.api.*
import template.webServer.route.RouteUtils.*
import zio.*

object PaymentRoutes {

  val routes: Route[PaymentApi & SessionConfig & Transaction[DomainError]] =
    "payment" /: Route.oneOf(
      (HttpMethod.POST / "create-intent").implement { _ =>
        for {
          token <- sessionToken
          clientSecret <- ZIO.serviceWithZIO[PaymentApi](_.createIntent(token))
        } yield HttpResponse.encodeJson(clientSecret)
      },
      (HttpMethod.GET / "accept-setup-intent").implement { _ =>
        for {
          token <- sessionToken
          setupIntentId <- HttpRequest.query.get[PM.ids.SetupIntentId]("setup_intent").mapError(_.toDomain)
          _ <- ZIO.serviceWithZIO[PaymentApi](_.acceptSetupIntent(token, setupIntentId))
        } yield HttpResponse.redirect("/page/account")
      },
      (HttpMethod.GET / "payment-methods").implement { _ =>
        for {
          token <- sessionToken
          paymentMethods <- ZIO.serviceWithZIO[PaymentApi](_.paymentMethods(token))
        } yield HttpResponse.encodeJson(paymentMethods.map(_.toApi))
      },
    )

}
