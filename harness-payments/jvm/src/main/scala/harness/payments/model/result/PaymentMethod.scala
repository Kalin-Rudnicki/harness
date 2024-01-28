package harness.payments.model.result

import com.stripe.model.PaymentMethod as StripePaymentMethod
import harness.core.*
import harness.payments.model.ids.*
import harness.zio.*
import zio.*

final case class PaymentMethod(
    id: PaymentMethodId,
    typeString: String,
    typeDetails: Option[TypeDetails],
)
object PaymentMethod {

  private implicit class StripePaymentMethodOps(paymentMethod: StripePaymentMethod) {

    def requireField[A](fieldName: String)(f: StripePaymentMethod => A): HTask[A] =
      ZIO.getOrFailWith(HError.InternalDefect(s"StripePaymentMethod is missing required field '$fieldName'"))(Option(f(paymentMethod)))

  }

  def fromStripe(paymentMethod: StripePaymentMethod): HRIO[Logger, PaymentMethod] =
    for {
      pmType <- paymentMethod.requireField("type")(_.getType)
      typeDetails <- pmType match {
        case "card" =>
          paymentMethod
            .requireField("card")(_.getCard)
            .map { card =>
              TypeDetails.Card(
                brand = card.getBrand,
                expMonth = card.getExpMonth.toInt,
                expYear = card.getExpYear.toInt,
                last4 = card.getLast4,
              )
            }
            .asSome
        case _ =>
          Logger.log.warning(s"Unknown payment method type: $pmType", "alert" -> "stripe payment method type").as(None)
      }
    } yield PaymentMethod(
      id = PaymentMethodId(paymentMethod.getId),
      typeString = pmType,
      typeDetails = typeDetails,
    )

}
