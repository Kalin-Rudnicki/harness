package harness.archive.model.user

import zio.json.*

final case class SignUp(
    firstName: String,
    lastName: String,
    username: String,
    password: String,
    email: String,
)
object SignUp {
  implicit val jsonCodec: JsonCodec[SignUp] = DeriveJsonCodec.gen
}
