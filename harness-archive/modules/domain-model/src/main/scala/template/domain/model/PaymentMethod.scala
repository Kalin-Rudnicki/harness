package template.domain.model

import harness.payments.model.ids as StripeIds
import harness.payments.model as PM
import template.api.model as Api

final case class PaymentMethod(
    id: Api.paymentMethod.PaymentMethodId,
    userId: Api.user.UserId,
    stripeId: StripeIds.PaymentMethodId,
    typeString: String,
    typeDetails: Option[PM.result.TypeDetails],
) { self =>

  def toApi: Api.paymentMethod.PaymentMethod =
    Api.paymentMethod.PaymentMethod(
      id = self.id,
      userId = self.userId,
      typeString = self.typeString,
      typeDetails = self.typeDetails,
    )

}
