package template.api.service

import cats.syntax.option.*
import harness.payments.model.ids.*
import harness.payments.model as PM
import harness.payments.service.PaymentProcessor
import harness.zio.*
import template.api.model as Api
import template.domain.model.*
import template.domain.session.*
import template.domain.storage.*
import zio.*

final case class PaymentApi(
    sessionService: SessionService,
    userStorage: UserStorage,
    paymentMethodStorage: PaymentMethodStorage,
    paymentProcessor: PaymentProcessor,
) {

  private def getOrCreateStripeCustomer(user: User): IO[DomainError, CustomerId] =
    ZIO.succeed(user.stripeCustomerId).someOrElseZIO {
      Logger.log.info(s"Creating stripe customer", "userId" -> user.id.toUUID) *>
        paymentProcessor
          .createCustomer(PM.create.Customer(s"${user.firstName} ${user.lastName}".some, user.email.some))
          .mapError(DomainError.UnexpectedPaymentError(_))
          .tap(customerId => userStorage.setStripeCustomerId(user.id, customerId.some))
    }

  def createIntent(token: Api.user.UserToken): IO[DomainError, ClientSecret] =
    for {
      user <- sessionService.getUser(token)
      _ <- Logger.log.info("Attempting to create setup intent", "userId" -> user.id.toUUID)
      customerId <- getOrCreateStripeCustomer(user)
      setupIntent <- paymentProcessor.createSetupIntent(PM.create.SetupIntent(customerId, None)).mapError(DomainError.UnexpectedPaymentError(_))
    } yield setupIntent.clientSecret

  def acceptIntent(token: Api.user.UserToken, setupIntentId: SetupIntentId): IO[DomainError, Unit] =
    for {
      user <- sessionService.getUser(token)
      _ <- Logger.log.info("Attempting to accept setup intent", "userId" -> user.id.toUUID)
      setupIntent <- paymentProcessor.getSetupIntent(setupIntentId).mapError(DomainError.UnexpectedPaymentError(_))
      paymentMethod <- paymentProcessor.getPaymentMethod(setupIntent.paymentMethodId).mapError(DomainError.UnexpectedPaymentError(_))
      domainPaymentMethod = PaymentMethod(Api.paymentMethod.PaymentMethodId.gen, user.id, paymentMethod.id, paymentMethod.typeString, paymentMethod.typeDetails)
      _ <- paymentMethodStorage.insert(domainPaymentMethod)
      _ <- Logger.log.info("Successfully created payment method", "paymentMethodId" -> domainPaymentMethod.id)
    } yield ()

  def paymentMethods(token: Api.user.UserToken): IO[DomainError, Chunk[PaymentMethod]] =
    for {
      user <- sessionService.getUser(token)
      _ <- Logger.log.info("Attempting to get payment methods", "userId" -> user.id.toUUID)
      paymentMethods <- paymentMethodStorage.getForUser(user.id)
    } yield paymentMethods

}
object PaymentApi {

  val layer: URLayer[UserStorage & PaymentMethodStorage & PaymentProcessor & SessionService, PaymentApi] =
    ZLayer.fromFunction { PaymentApi.apply }

}
