package harness.payments.model.result

import com.stripe.model.SetupIntent as StripeSetupIntent
import harness.payments.error.PaymentError
import harness.payments.model.ids.*
import zio.*

object SetupIntent {

  private final case class Raw(
      id: SetupIntentId,
      clientSecret: ClientSecret,
      paymentMethodId: Option[PaymentMethodId],
      createdEpochMS: Long,
      description: Option[String],
  ) {

    def requireField[A](fieldName: String)(f: this.type => Option[A]): IO[PaymentError.InternalDefect, A] =
      ZIO.getOrFailWith(PaymentError.InternalDefect(s"SetupIntent is missing required field '$fieldName'"))(f(this))

  }
  private object Raw {

    def fromStripe(setupIntent: StripeSetupIntent): SetupIntent.Raw =
      Raw(
        SetupIntentId(setupIntent.getId),
        ClientSecret(setupIntent.getClientSecret),
        Option(setupIntent.getPaymentMethod).map(PaymentMethodId(_)),
        setupIntent.getCreated,
        Option(setupIntent.getDescription),
      )

  }

  final case class Empty(
      id: SetupIntentId,
      clientSecret: ClientSecret,
      createdEpochMS: Long,
      description: Option[String],
  )
  object Empty {

    def fromStripe(setupIntent: StripeSetupIntent): UIO[SetupIntent.Empty] = {
      val raw = SetupIntent.Raw.fromStripe(setupIntent)

      ZIO.succeed(Empty(raw.id, raw.clientSecret, raw.createdEpochMS, raw.description))
    }

  }

  final case class Initialized(
      id: SetupIntentId,
      clientSecret: ClientSecret,
      paymentMethodId: PaymentMethodId,
      createdEpochMS: Long,
      description: Option[String],
  )
  object Initialized {

    def fromStripe(setupIntent: StripeSetupIntent): IO[PaymentError.InternalDefect, SetupIntent.Initialized] = {
      val raw = SetupIntent.Raw.fromStripe(setupIntent)

      for {
        paymentMethodId <- raw.requireField("paymentMethodId")(_.paymentMethodId)
      } yield Initialized(raw.id, raw.clientSecret, paymentMethodId, raw.createdEpochMS, raw.description)
    }

  }

}
