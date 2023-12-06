package template.model.user

import harness.email.EmailAddress
import zio.json.*

final case class SignUp(
    firstName: String,
    lastName: String,
    username: String,
    password: String,
    email: EmailAddress,
)
object SignUp {
  implicit val jsonCodec: JsonCodec[SignUp] = DeriveJsonCodec.gen
}
