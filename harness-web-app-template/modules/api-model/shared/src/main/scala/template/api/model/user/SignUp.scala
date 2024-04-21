package template.api.model.user

import harness.email.EmailAddress
import harness.schema.*

final case class SignUp(
    firstName: String,
    lastName: String,
    username: String,
    password: String,
    email: EmailAddress,
)
object SignUp {
  implicit val schema: JsonSchema[SignUp] = JsonSchema.derive
}
