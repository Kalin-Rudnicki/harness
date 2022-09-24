package template.model.user

import zio.json.*

final case class User(
    firstName: String,
    lastName: String,
    username: String,
    email: String,
)
object User {
  implicit val jsonCodec: JsonCodec[User] = DeriveJsonCodec.gen
}
