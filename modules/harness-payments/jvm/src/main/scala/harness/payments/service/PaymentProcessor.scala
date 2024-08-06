package harness.payments.service

import com.stripe.Stripe
import com.stripe.model.*
import com.stripe.param.*
import harness.payments.error.PaymentError
import harness.payments.model.ids.*
import harness.payments.model as PM
import harness.zio.*
import scala.jdk.CollectionConverters.*
import scala.reflect.ClassTag
import zio.*
import zio.json.*

trait PaymentProcessor {
  def createCustomer(customer: PM.create.Customer): IO[PaymentError, CustomerId]
  def createSetupIntent(create: PM.create.SetupIntent): IO[PaymentError, PM.result.SetupIntent.Empty]
  def getSetupIntent(setupIntentId: SetupIntentId): IO[PaymentError, PM.result.SetupIntent.Initialized]
  def getPaymentMethod(paymentMethodId: PaymentMethodId): IO[PaymentError, PM.result.PaymentMethod]
  def processPayment(payment: PM.create.Payment): IO[PaymentError, PaymentId] // TODO (KR) : payment?
}
object PaymentProcessor {

  // =====|  |=====

  def createCustomer(customer: PM.create.Customer): ZIO[PaymentProcessor & Logger & Telemetry, PaymentError, CustomerId] =
    ZIO.serviceWithZIO[PaymentProcessor](_.createCustomer(customer))

  def createSetupIntent(create: PM.create.SetupIntent): ZIO[PaymentProcessor & Logger & Telemetry, PaymentError, PM.result.SetupIntent.Empty] =
    ZIO.serviceWithZIO[PaymentProcessor](_.createSetupIntent(create))

  def getSetupIntent(setupIntentId: SetupIntentId): ZIO[PaymentProcessor & Logger & Telemetry, PaymentError, PM.result.SetupIntent.Initialized] =
    ZIO.serviceWithZIO[PaymentProcessor](_.getSetupIntent(setupIntentId))

  def getPaymentMethod(paymentMethodId: PaymentMethodId): ZIO[PaymentProcessor & Logger & Telemetry, PaymentError, PM.result.PaymentMethod] =
    ZIO.serviceWithZIO[PaymentProcessor](_.getPaymentMethod(paymentMethodId))

  def processPayment(payment: PM.create.Payment): ZIO[PaymentProcessor & Logger & Telemetry, PaymentError, PaymentId] =
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

    private def runStripe[R, E, A](method: String)(effect: => ZIO[R, E, A]): ZIO[R, E, A] =
      (ZIO.attempt { Stripe.apiKey = config.secretKey }.orDie *> effect)
        .telemetrize(s"StripePaymentProcessor call", "stripe-method" -> method)

    private def safeWrapStripeParams[A](thunk: => A)(implicit ct: ClassTag[A]): IO[PaymentError.FailedToCreateParams, A] =
      ZIO.attempt { thunk }.mapError(PaymentError.FailedToCreateParams(ct.runtimeClass.getName, _))

    private def safeWrapStripeCall[A](call: String)(thunk: => A): IO[PaymentError.FailedToMakeCall, A] =
      ZIO.attempt { thunk }.mapError(PaymentError.FailedToMakeCall(call, _))

    @scala.annotation.unused
    private def logModel[A](mainLabel: String, a: A)(fields: (String, A => Any)*): URIO[Logger, Unit] = {
      val fieldsStr = fields.map { case (label, f) => s"\n  - $label : ${f(a)}" }.mkString

      Logger.log.info(s"--- $mainLabel ---$fieldsStr")
    }

    override def createCustomer(customer: PM.create.Customer): IO[PaymentError, CustomerId] =
      runStripe("createCustomer") {
        for {
          params <- safeWrapStripeParams {
            CustomerCreateParams
              .builder()
              .setOpt(customer.name)(_.setName(_))
              .setOpt(customer.email)((b, email) => b.setEmail(email.unwrap))
              .build()
          }
          customer <- safeWrapStripeCall("Customer.create") { Customer.create(params) }
        } yield CustomerId(customer.getId)
      }

    override def createSetupIntent(create: PM.create.SetupIntent): IO[PaymentError, PM.result.SetupIntent.Empty] =
      runStripe("createSetupIntent") {
        for {
          params <- safeWrapStripeParams {
            SetupIntentCreateParams
              .builder()
              .setCustomer(create.customerId.value)
              .setOpt(create.description)(_.setDescription(_))
              .build()
          }
          setupIntent <- safeWrapStripeCall("SetupIntent.create") { SetupIntent.create(params) }
          apiSetupIntent <- PM.result.SetupIntent.Empty.fromStripe(setupIntent)
        } yield apiSetupIntent
      }

    override def getSetupIntent(setupIntentId: SetupIntentId): IO[PaymentError, PM.result.SetupIntent.Initialized] =
      runStripe("getSetupIntent") {
        for {
          setupIntent <- safeWrapStripeCall("SetupIntent.retrieve") { SetupIntent.retrieve(setupIntentId.value) }
          apiSetupIntent <- PM.result.SetupIntent.Initialized.fromStripe(setupIntent)
        } yield apiSetupIntent
      }

    override def getPaymentMethod(paymentMethodId: PaymentMethodId): IO[PaymentError, PM.result.PaymentMethod] =
      runStripe("getPaymentMethod") {
        for {
          paymentMethod <- safeWrapStripeCall("PaymentMethod.retrieve") { PaymentMethod.retrieve(paymentMethodId.value) }
          apiPaymentMethod <- PM.result.PaymentMethod.fromStripe(paymentMethod)
        } yield apiPaymentMethod
      }

    override def processPayment(payment: PM.create.Payment): IO[PaymentError, PaymentId] =
      runStripe("processPayment") {
        for {
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
        } yield PaymentId(paymentResult.getId)
      }

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

    override def createCustomer(customer: PM.create.Customer): IO[PaymentError, CustomerId] =
      ZIO.dieMessage("unimplemented : MockPaymentProcessor.createCustomer") // TODO (KR) :

    override def createSetupIntent(create: PM.create.SetupIntent): IO[PaymentError, PM.result.SetupIntent.Empty] =
      ZIO.dieMessage("unimplemented : MockPaymentProcessor.createSetupIntent") // TODO (KR) :

    override def getSetupIntent(setupIntentId: SetupIntentId): IO[PaymentError, PM.result.SetupIntent.Initialized] =
      ZIO.dieMessage("unimplemented : MockPaymentProcessor.getSetupIntent") // TODO (KR) :

    override def getPaymentMethod(paymentMethodId: PaymentMethodId): IO[PaymentError, PM.result.PaymentMethod] =
      ZIO.dieMessage("unimplemented : MockPaymentProcessor.getPaymentMethod") // TODO (KR) :

    override def processPayment(payment: PM.create.Payment): IO[PaymentError, PaymentId] =
      ZIO.dieMessage("unimplemented : MockPaymentProcessor.processPayment") // TODO (KR) :

  }
  object MockPaymentProcessor {

    val layer: ULayer[PaymentProcessor.MockPaymentProcessor] =
      ZLayer.succeed { MockPaymentProcessor() }

  }

}
