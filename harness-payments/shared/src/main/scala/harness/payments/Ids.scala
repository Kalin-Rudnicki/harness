package harness.payments

import harness.pk.StringId

type CustomerId = CustomerId.Id
object CustomerId extends StringId

type ClientSecret = ClientSecret.Id
object ClientSecret extends StringId

type PaymentMethodId = PaymentMethodId.Id
object PaymentMethodId extends StringId
