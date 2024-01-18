package harness.payments

import harness.email.*

final case class Charge(
    amountInCents: Long,
    currency: Currency,
    description: String,
    source: PaymentSourceId,
    email: Option[EmailAddress],
)
