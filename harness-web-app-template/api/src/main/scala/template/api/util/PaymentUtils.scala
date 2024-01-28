package template.api.util

import cats.syntax.option.*
import harness.payments.model.ids.*
import harness.payments.model as PM
import harness.payments.service.PaymentProcessor
import harness.zio.*
import template.api.db.model as M
import template.api.service.storage.UserStorage
import zio.*

object PaymentUtils {

  def getOrCreateStripeCustomer(user: M.User.Identity): HRIO[PaymentProcessor & UserStorage & Logger & Telemetry, CustomerId] =
    ZIO.succeed(user.stripeCustomerId).someOrElseZIO {
      Logger.log.info(s"Creating stripe customer for user: ${user.show}") *>
        PaymentProcessor
          .createCustomer(PM.create.Customer(s"${user.firstName} ${user.lastName}".some, user.email.some))
          .tap(customerId => UserStorage.setStripeCustomerId(user.id, customerId.some))
    }

}
