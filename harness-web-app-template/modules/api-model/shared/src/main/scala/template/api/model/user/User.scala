package template.api.model.user

import harness.email.EmailAddress
import harness.schema.*

final case class User(
    id: UserId,
    firstName: String,
    lastName: String,
    username: String,
    email: EmailAddress,
    emailIsVerified: Boolean,
)
object User {
  implicit val schema: JsonSchema[User] = JsonSchema.derive
}
