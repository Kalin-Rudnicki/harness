package harness.payments.model.ids

import harness.pk.StringId

type ChargeId = ChargeId.Id
object ChargeId extends StringId

type PaymentId = PaymentId.Id
object PaymentId extends StringId

type PaymentSourceId = PaymentSourceId.Id
object PaymentSourceId extends StringId

type SetupIntentId = SetupIntentId.Id
object SetupIntentId extends StringId
