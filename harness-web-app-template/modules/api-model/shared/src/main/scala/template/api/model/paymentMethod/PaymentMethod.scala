package template.api.model.paymentMethod

import harness.payments.model as PM
import harness.schema.*
import template.api.model as Api
import zio.json.*

final case class PaymentMethod(
    id: PaymentMethodId,
    userId: Api.user.UserId,
    typeString: String,
    typeDetails: Option[PM.result.TypeDetails],
)
object PaymentMethod {
  implicit val schema: JsonSchema[PaymentMethod] = JsonSchema.derived
}
