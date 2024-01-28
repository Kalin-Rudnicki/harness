package harness.payments.model.create

import harness.email.*

final case class Customer(
    name: Option[String],
    email: Option[EmailAddress],
)
