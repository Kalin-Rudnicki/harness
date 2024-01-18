package template.api.routes

import cats.data.NonEmptyList
import cats.syntax.option.*
import harness.core.*
import harness.email.*
import harness.http.server.{given, *}
import harness.payments.{Payment as ApiPayment, *}
import harness.sql.*
import harness.sql.query.Transaction
import harness.web.*
import harness.zio.*
import java.util.UUID
import scala.jdk.CollectionConverters.*
import template.api.db.model as M
import template.api.service.email.*
import template.api.service.storage.*
import template.api.util.*
import template.model as D
import zio.*

object Payment {

  private implicit class DbUserOps(dbUser: M.User.Identity) {
    def getCustomerId: HTask[CustomerId] =
      ZIO.getOrFailWith(HError.InternalDefect(s"User ${dbUser.show} does not have customerId"))(dbUser.stripeCustomerId)
  }

  val routes: Route[UserStorage & SessionStorage & PaymentProcessor & Transaction] =
    "payment" /: Route.oneOf(
      (HttpMethod.POST / "create-intent").implement { _ =>
        Transaction.inTransaction {
          for {
            dbUser <- SessionUtils.userFromSessionToken
            customerId <- PaymentUtils.getOrCreateStripeCustomer(dbUser)
            clientSecret <- PaymentProcessor.createSetupIntent(customerId)
          } yield HttpResponse.encodeJson(clientSecret)
        }
      },
      (HttpMethod.GET / "accept-setup-intent").implement { _ =>
        Transaction.inTransaction {
          for {
            dbUser <- SessionUtils.userFromSessionToken
            // setupIntent <- HttpRequest.query.get[String]("setup_intent")
            // setupIntentClientSecret <- HttpRequest.query.get[String]("setup_intent_client_secret")
            // redirectStatus <- HttpRequest.query.get[String]("redirect_status")

            customerId <- dbUser.getCustomerId
            paymentMethods <- PaymentProcessor.getPaymentMethods(customerId)

            _ <- Logger.log.info(paymentMethods.map { pm => s"${pm.getId} (${pm.getType}): ${pm.getMetadata.asScala}" }.mkString("\n"))

            paymentId <- PaymentProcessor.processPayment(
              ApiPayment(
                customerId = customerId,
                paymentMethodId = PaymentMethodId(paymentMethods.head.getId),
                amountInCents = 1234L,
                currency = Currency.USD,
                description = s"Charge ${dbUser.show} - ${UUID.randomUUID}",
                email = dbUser.email.some,
              ),
            )
            _ <- Logger.log.info(s"Created payment : $paymentId")

            // TODO (KR) :
          } yield HttpResponse.fromHttpCode.Ok // .redirect("/page")
        }
      },
    )

}
