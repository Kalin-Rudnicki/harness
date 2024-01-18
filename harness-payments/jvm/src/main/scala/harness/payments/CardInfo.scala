package harness.payments

final case class CardInfo(
    cardNumber: String,
    expMonth: Int,
    expYear: Int,
    cvc: String
)
