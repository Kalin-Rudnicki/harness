package harness.payments.model.create

import harness.email.*
import harness.payments.model.Currency
import harness.payments.model.ids.*

final case class Payment(
    customerId: CustomerId,
    paymentMethodId: PaymentMethodId,
    amountInCents: Long,
    currency: Currency,
    description: String,
    email: Option[EmailAddress],
)
