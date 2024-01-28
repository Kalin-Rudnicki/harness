package harness.payments.model.result

import zio.json.*

sealed trait TypeDetails
object TypeDetails {

  // TODO (KR) : other fields?
  //           : - country
  //           : - funding
  //           : - description
  //           : - iin
  //           : - issuer
  //           : - wallet
  final case class Card(
      brand: String,
      expMonth: Int,
      expYear: Int,
      last4: String,
  ) extends TypeDetails

  implicit val jsonCodec: JsonCodec[TypeDetails] = DeriveJsonCodec.gen

}
