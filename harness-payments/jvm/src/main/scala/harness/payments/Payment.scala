package harness.payments

import harness.email.*

final case class Payment(
    customerId: CustomerId,
    paymentMethodId: PaymentMethodId,
    amountInCents: Long,
    currency: Currency,
    description: String,
    email: Option[EmailAddress]
)
