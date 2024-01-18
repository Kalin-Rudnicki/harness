package harness.payments

import harness.email.*

final case class CreateCustomer(
    name: Option[String],
    email: Option[EmailAddress],
)
