package template.model.user

import zio.json.*

import java.util.UUID

final case class User(
    id: UUID,
    firstName: String,
    lastName: String,
    username: String,
    email: String,
)
object User {
  implicit val jsonCodec: JsonCodec[User] = DeriveJsonCodec.gen
}
