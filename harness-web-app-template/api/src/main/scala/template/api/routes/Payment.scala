package template.api.routes

import harness.core.*
import harness.http.server.{given, *}
import harness.payments.model.ids.*
import harness.payments.model as PM
import harness.payments.service.PaymentProcessor
import harness.sql.*
import harness.sql.query.Transaction
import harness.web.*
import harness.zio.*
import template.api.db.model as M
import template.api.service.storage.*
import template.api.util.*
import template.model as D
import zio.*

object Payment {

  implicit class DbUserOps(dbUser: M.User.Identity) {
    def getCustomerId: HTask[CustomerId] =
      ZIO.getOrFailWith(HError.InternalDefect(s"User ${dbUser.show} does not have customerId"))(dbUser.stripeCustomerId)
  }

  val routes: Route[UserStorage & SessionStorage & PaymentMethodStorage & PaymentProcessor & Transaction] =
    "payment" /: Route.oneOf(
      (HttpMethod.POST / "create-intent").implement { _ =>
        Transaction.inTransaction {
          for {
            dbUser <- SessionUtils.userFromSessionToken
            customerId <- PaymentUtils.getOrCreateStripeCustomer(dbUser)
            setupIntent <- PaymentProcessor.createSetupIntent(PM.create.SetupIntent(customerId, None))
          } yield HttpResponse.encodeJson(setupIntent.clientSecret)
        }
      },
      (HttpMethod.GET / "accept-setup-intent").implement { _ =>
        Transaction.inTransaction {
          for {
            dbUser <- SessionUtils.userFromSessionToken

            _ <- HttpRequest.query.logAll(Logger.LogLevel.Important)
            setupIntentId <- HttpRequest.query.get[PM.ids.SetupIntentId]("setup_intent")
            // setupIntentClientSecret <- HttpRequest.query.get[ClientSecret]("setup_intent_client_secret")
            // redirectStatus <- HttpRequest.query.get[String]("redirect_status")

            setupIntent <- PaymentProcessor.getSetupIntent(setupIntentId)
            paymentMethod <- PaymentProcessor.getPaymentMethod(setupIntent.paymentMethodId)

            dbPaymentMethod = new M.PaymentMethod.Identity(M.PaymentMethod.Id.gen, dbUser.id, paymentMethod.id, paymentMethod.typeString, paymentMethod.typeDetails)
            _ <- PaymentMethodStorage.insert(dbPaymentMethod)
            _ <- Logger.log.info(s"Successfully created payment-method ${dbPaymentMethod.id} for user ${dbUser.show}")

            // TODO (KR) : where to redirect to?
          } yield HttpResponse.redirect("/page/account")
        }
      },
      (HttpMethod.GET / "payment-methods").implement { _ =>
        Transaction.inTransaction {
          for {
            dbUser <- SessionUtils.userFromSessionToken
            paymentMethods <- PaymentMethodStorage.getForUser(dbUser.id)
          } yield HttpResponse.encodeJson(paymentMethods.map(DbToDomain.paymentMethod))
        }
      },
    )

}
