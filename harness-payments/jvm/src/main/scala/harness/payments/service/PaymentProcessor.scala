package harness.payments.service

import com.stripe.Stripe
import com.stripe.model.*
import com.stripe.param.*
import harness.core.*
import harness.email.*
import harness.payments.model.ids.*
import harness.payments.model as PM
import harness.zio.*
import scala.jdk.CollectionConverters.*
import scala.reflect.ClassTag
import zio.*
import zio.json.*

trait PaymentProcessor {
  def createCustomer(customer: PM.create.Customer): HRIO[Logger & Telemetry, CustomerId]
  def createSetupIntent(create: PM.create.SetupIntent): HRIO[Logger & Telemetry, PM.result.SetupIntent.Empty]
  def getSetupIntent(setupIntentId: SetupIntentId): HRIO[Logger & Telemetry, PM.result.SetupIntent.Initialized]
  def getPaymentMethod(paymentMethodId: PaymentMethodId): HRIO[Logger & Telemetry, PM.result.PaymentMethod]
  def processPayment(payment: PM.create.Payment): HRIO[Logger & Telemetry, PaymentId] // TODO (KR) : payment?
}
object PaymentProcessor {

  // =====|  |=====

  def createCustomer(customer: PM.create.Customer): HRIO[PaymentProcessor & Logger & Telemetry, CustomerId] =
    ZIO.serviceWithZIO[PaymentProcessor](_.createCustomer(customer))

  def createSetupIntent(create: PM.create.SetupIntent): HRIO[PaymentProcessor & Logger & Telemetry, PM.result.SetupIntent.Empty] =
    ZIO.serviceWithZIO[PaymentProcessor](_.createSetupIntent(create))

  def getSetupIntent(setupIntentId: SetupIntentId): HRIO[PaymentProcessor & Logger & Telemetry, PM.result.SetupIntent.Initialized] =
    ZIO.serviceWithZIO[PaymentProcessor](_.getSetupIntent(setupIntentId))

  def getPaymentMethod(paymentMethodId: PaymentMethodId): HRIO[PaymentProcessor & Logger & Telemetry, PM.result.PaymentMethod] =
    ZIO.serviceWithZIO[PaymentProcessor](_.getPaymentMethod(paymentMethodId))

  def processPayment(payment: PM.create.Payment): HRIO[PaymentProcessor & Logger & Telemetry, PaymentId] =
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
      ZIO.hAttempt { Stripe.apiKey = config.secretKey }

    private def safeWrapStripeParams[A](thunk: => A)(implicit ct: ClassTag[A]): HTask[A] =
      ZIO.hAttempt { thunk }.mapError(HError.SystemFailure(s"Error creating stripe params: ${ct.runtimeClass.getName}", _))

    private def safeWrapStripeCall[A](call: String)(thunk: => A): HTask[A] =
      ZIO.hAttempt { thunk }.mapError(HError.SystemFailure(s"Error making stripe call: $call", _))

    private def logModel[A](mainLabel: String, a: A)(fields: (String, A => Any)*): URIO[Logger, Unit] = {
      val fieldsStr = fields.map { case (label, f) => s"\n  - $label : ${f(a)}" }.mkString

      Logger.log.info(s"--- $mainLabel ---$fieldsStr")
    }

    override def createCustomer(customer: PM.create.Customer): HRIO[Logger & Telemetry, CustomerId] =
      (for {
        _ <- initStripeMetadata
        params <- safeWrapStripeParams {
          CustomerCreateParams
            .builder()
            .setOpt(customer.name)(_.setName(_))
            .setOpt(customer.email)((b, email) => b.setEmail(email.unwrap))
            .build()
        }
        customer <- safeWrapStripeCall("Customer.create") { Customer.create(params) }
      } yield CustomerId(customer.getId)).telemetrize("StripePaymentProcessor - createCustomer")

    override def createSetupIntent(create: PM.create.SetupIntent): HRIO[Logger & Telemetry, PM.result.SetupIntent.Empty] =
      (for {
        _ <- initStripeMetadata
        params <- safeWrapStripeParams {
          SetupIntentCreateParams
            .builder()
            .setCustomer(create.customerId.value)
            .setOpt(create.description)(_.setDescription(_))
            .build()
        }
        setupIntent <- safeWrapStripeCall("SetupIntent.create") { SetupIntent.create(params) }
        apiSetupIntent <- PM.result.SetupIntent.Empty.fromStripe(setupIntent)
      } yield apiSetupIntent).telemetrize("StripePaymentProcessor - createSetupIntent")

    override def getSetupIntent(setupIntentId: SetupIntentId): HRIO[Logger & Telemetry, PM.result.SetupIntent.Initialized] =
      (for {
        setupIntent <- safeWrapStripeCall("SetupIntent.retrieve") { SetupIntent.retrieve(setupIntentId.value) }
        apiSetupIntent <- PM.result.SetupIntent.Initialized.fromStripe(setupIntent)
      } yield apiSetupIntent).telemetrize("StripePaymentProcessor - getSetupIntent")

    override def getPaymentMethod(paymentMethodId: PaymentMethodId): HRIO[Logger & Telemetry, PM.result.PaymentMethod] =
      (for {
        _ <- initStripeMetadata
        paymentMethod <- safeWrapStripeCall("PaymentMethod.retrieve") { PaymentMethod.retrieve(paymentMethodId.value) }
        apiPaymentMethod <- PM.result.PaymentMethod.fromStripe(paymentMethod)
      } yield apiPaymentMethod).telemetrize("StripePaymentProcessor - getPaymentMethod")

    override def processPayment(payment: PM.create.Payment): HRIO[Logger & Telemetry, PaymentId] =
      (for {
        _ <- initStripeMetadata
        params <- safeWrapStripeParams {
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
        paymentResult <- safeWrapStripeCall("PaymentIntent.create") { PaymentIntent.create(params) }
      } yield PaymentId(paymentResult.getId)).telemetrize("StripePaymentProcessor - processPayment")

  }
  object StripePaymentProcessor {

    val layer: URLayer[StripePaymentProcessor.Config, PaymentProcessor.StripePaymentProcessor] =
      ZLayer.fromFunction { StripePaymentProcessor.apply }

    final case class Config(
        publishableKey: String,
        secretKey: String,
    )
    object Config {
      implicit val jsonCodec: JsonCodec[Config] = DeriveJsonCodec.gen
    }

  }

  // TODO (KR) :
  final case class MockPaymentProcessor() extends PaymentProcessor {

    override def createCustomer(customer: PM.create.Customer): HRIO[Logger & Telemetry, CustomerId] =
      ZIO.fail(HError.???("MockPaymentProcessor.createCustomer")) // TODO (KR) :

    override def createSetupIntent(create: PM.create.SetupIntent): HRIO[Logger & Telemetry, PM.result.SetupIntent.Empty] =
      ZIO.fail(HError.???("MockPaymentProcessor.createSetupIntent")) // TODO (KR) :

    override def getSetupIntent(setupIntentId: SetupIntentId): HRIO[Logger & Telemetry, PM.result.SetupIntent.Initialized] =
      ZIO.fail(HError.???("MockPaymentProcessor.getSetupIntent")) // TODO (KR) :

    override def getPaymentMethod(paymentMethodId: PaymentMethodId): HRIO[Logger & Telemetry, PM.result.PaymentMethod] =
      ZIO.fail(HError.???("MockPaymentProcessor.getPaymentMethod")) // TODO (KR) :

    override def processPayment(payment: PM.create.Payment): HRIO[Logger & Telemetry, PaymentId] =
      ZIO.fail(HError.???("MockPaymentProcessor.processPayment")) // TODO (KR) :

  }
  object MockPaymentProcessor {

    val layer: ULayer[PaymentProcessor.MockPaymentProcessor] =
      ZLayer.succeed { MockPaymentProcessor() }

  }

}
