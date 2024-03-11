package harness.archive.api.model.user

import harness.email.EmailAddress
import zio.json.*

final case class User(
    id: UserId,
    firstName: String,
    lastName: String,
    username: String,
    email: EmailAddress,
)
object User {
  implicit val jsonCodec: JsonCodec[User] = DeriveJsonCodec.gen
}
