package template.model.paymentMethod

import harness.payments.model as PM
import template.model as D
import zio.json.*

final case class PaymentMethod(
    id: PaymentMethodId,
    userId: D.user.UserId,
    typeString: String,
    typeDetails: Option[PM.result.TypeDetails],
)
object PaymentMethod {
  implicit val jsonCodec: JsonCodec[PaymentMethod] = DeriveJsonCodec.gen
}
