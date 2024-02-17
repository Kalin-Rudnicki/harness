package harness.payments.model.create

import harness.payments.model.ids.*

final case class SetupIntent(
    customerId: CustomerId,
    description: Option[String],
)
