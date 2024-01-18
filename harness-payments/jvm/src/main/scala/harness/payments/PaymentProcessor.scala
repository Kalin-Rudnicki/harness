package harness.payments

import com.stripe.Stripe
import com.stripe.model.*
import com.stripe.param.*
import harness.core.*
import harness.email.*
import harness.payments.{Charge as ApiCharge, Payment as ApiPayment}
import harness.zio.*
import scala.jdk.CollectionConverters.*
import zio.*
import zio.json.*

trait PaymentProcessor {
  def createCustomer(customer: CreateCustomer): HRIO[Logger & Telemetry, CustomerId]
  def createSetupIntent(customerId: CustomerId): HRIO[Logger & Telemetry, ClientSecret]
  def getPaymentMethods(customerId: CustomerId): HRIO[Logger & Telemetry, List[PaymentMethod]]
  def processCharge(charge: ApiCharge): HRIO[Logger & Telemetry, ChargeId]
  def processPayment(payment: ApiPayment): HRIO[Logger & Telemetry, PaymentId]
}
object PaymentProcessor {

  // =====|  |=====

  def createCustomer(customer: CreateCustomer): HRIO[PaymentProcessor & Logger & Telemetry, CustomerId] =
    ZIO.serviceWithZIO[PaymentProcessor](_.createCustomer(customer))

  def createSetupIntent(customerId: CustomerId): HRIO[PaymentProcessor & Logger & Telemetry, ClientSecret] =
    ZIO.serviceWithZIO[PaymentProcessor](_.createSetupIntent(customerId))

  def getPaymentMethods(customerId: CustomerId): HRIO[PaymentProcessor & Logger & Telemetry, List[PaymentMethod]] =
    ZIO.serviceWithZIO[PaymentProcessor](_.getPaymentMethods(customerId))

  def processCharge(charge: ApiCharge): HRIO[PaymentProcessor & Logger & Telemetry, ChargeId] =
    ZIO.serviceWithZIO[PaymentProcessor](_.processCharge(charge))

  def processPayment(payment: ApiPayment): HRIO[PaymentProcessor & Logger & Telemetry, PaymentId] =
    ZIO.serviceWithZIO[PaymentProcessor](_.processPayment(payment))

  // =====|  |=====

  final case class StripePaymentProcessor(
      config: StripePaymentProcessor.Config,
  ) extends PaymentProcessor {

    private implicit class BuilderOps[B](ccp: B) {
      def setOpt[A](a: Option[A])(f: (B, A) => B): B =
        a match {
          case Some(value) => f(ccp, value)
          case None        => ccp
        }
    }

    private val initStripeMetadata: HTask[Unit] =
      ZIO.hAttempt { Stripe.apiKey = config.apiKey }

    override def createCustomer(customer: CreateCustomer): HRIO[Logger & Telemetry, CustomerId] =
      (for {
        _ <- initStripeMetadata
        params <-
          ZIO
            .hAttempt {
              CustomerCreateParams
                .builder()
                .setOpt(customer.name)(_.setName(_))
                .setOpt(customer.email)((b, email) => b.setEmail(email.unwrap))
                .build()
            }
            .mapError(HError.SystemFailure("Unable to create stripe customer params", _))
        customer <-
          ZIO
            .hAttempt { Customer.create(params) }
            .mapError(HError.SystemFailure("Error creating stripe customer", _))
      } yield CustomerId(customer.getId)).telemetrize("StripePaymentProcessor - createCustomer")

    def createSetupIntent(customerId: CustomerId): HRIO[Logger & Telemetry, ClientSecret] =
      (for {
        _ <- initStripeMetadata
        params <-
          ZIO
            .hAttempt {
              SetupIntentCreateParams
                .builder()
                .setCustomer(customerId.value)
                .build()
            }
            .mapError(HError.SystemFailure("Unable to create stripe setup-intent params", _))
        setupIntent <-
          ZIO
            .hAttempt { SetupIntent.create(params) }
            .mapError(HError.SystemFailure("Error creating stripe setup-intent", _))
      } yield ClientSecret(setupIntent.getClientSecret)).telemetrize("StripePaymentProcessor - createSetupIntent")

    override def getPaymentMethods(customerId: CustomerId): HRIO[Logger & Telemetry, List[PaymentMethod]] =
      (for {
        _ <- initStripeMetadata
        params <-
          ZIO
            .hAttempt {
              PaymentMethodListParams
                .builder()
                .setCustomer(customerId.value)
                .build()
            }
            .mapError(HError.SystemFailure("Unable to create stripe payment-method-list params", _))
        result <-
          ZIO
            .hAttempt { PaymentMethod.list(params) }
            .mapError(HError.SystemFailure("Error getting stripe payment-method-list", _))
        list = result.getData.asScala.toList
        _ <- Logger.log.debug(s"Found ${list.size.pluralizeOn("payment method")}")
      } yield list).telemetrize("StripePaymentProcessor - getPaymentMethods")

    override def processCharge(charge: ApiCharge): HRIO[Logger & Telemetry, ChargeId] =
      (for {
        _ <- initStripeMetadata
        params <-
          ZIO
            .hAttempt {
              ChargeCreateParams
                .builder()
                .setAmount(charge.amountInCents)
                .setCurrency(charge.currency.toString.toLowerCase)
                .setDescription(charge.description)
                .setSource(charge.source.value)
                .setOpt(charge.email)((b, email) => b.setReceiptEmail(email.unwrap))
                .build()
            }
            .mapError(HError.SystemFailure("Unable to create stripe charge params", _))
        chargeResult <-
          ZIO
            .hAttempt { Charge.create(params) }
            .mapError(HError.SystemFailure("Error creating stripe charge", _))
      } yield ChargeId(chargeResult.getId)).telemetrize("StripePaymentProcessor - processCharge")

    def processPayment(payment: ApiPayment): HRIO[Logger & Telemetry, PaymentId] =
      (for {
        _ <- initStripeMetadata
        params <-
          ZIO
            .hAttempt {
              PaymentIntentCreateParams
                .builder()
                .setCurrency(payment.currency.toString.toLowerCase)
                .setAmount(payment.amountInCents)
                .setCustomer(payment.customerId.value)
                .setPaymentMethod(payment.paymentMethodId.value)
                .setDescription(payment.description)
                .setOpt(payment.email)((b, a) => b.setReceiptEmail(a.unwrap))
                .setConfirm(true)
                .setOffSession(true)
                .build()
              // TODO (KR) : returnUrl?
            }
            .mapError(HError.SystemFailure("Unable to create stripe payment params", _))
        paymentResult <-
          ZIO
            .hAttempt { PaymentIntent.create(params) }
            .mapError(HError.SystemFailure("Error creating stripe payment", _))
      } yield PaymentId(paymentResult.getId)).telemetrize("StripePaymentProcessor - processPayment")

  }
  object StripePaymentProcessor {

    val layer: URLayer[StripePaymentProcessor.Config, PaymentProcessor.StripePaymentProcessor] =
      ZLayer.fromFunction { StripePaymentProcessor.apply }

    final case class Config(apiKey: String)
    object Config {
      implicit val jsonCodec: JsonCodec[Config] = DeriveJsonCodec.gen
    }

  }

  // TODO (KR) :
  final case class TestPaymentProcessor() extends PaymentProcessor {

    override def createCustomer(customer: CreateCustomer): HRIO[Logger & Telemetry, CustomerId] =
      ZIO.fail(HError.???("TestPaymentProcessor.createCustomer")) // TODO (KR) :

    override def createSetupIntent(customerId: CustomerId): HRIO[Logger & Telemetry, ClientSecret] =
      ZIO.fail(HError.???("TestPaymentProcessor.createSetupIntent")) // TODO (KR) :

    override def getPaymentMethods(customerId: CustomerId): HRIO[Logger & Telemetry, List[PaymentMethod]] =
      ZIO.fail(HError.???("TestPaymentProcessor.getPaymentMethods")) // TODO (KR) :

    override def processCharge(charge: ApiCharge): HRIO[Logger & Telemetry, ChargeId] =
      ZIO.fail(HError.???("TestPaymentProcessor.processCharge")) // TODO (KR) :

    override def processPayment(payment: ApiPayment): HRIO[Logger & Telemetry, PaymentId] =
      ZIO.fail(HError.???("TestPaymentProcessor.processPayment")) // TODO (KR) :

  }
  object TestPaymentProcessor {

    val layer: ULayer[PaymentProcessor.TestPaymentProcessor] =
      ZLayer.succeed { TestPaymentProcessor() }

  }

}
