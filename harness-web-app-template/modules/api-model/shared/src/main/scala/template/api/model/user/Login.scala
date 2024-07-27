package template.api.model.user

import harness.schema.*

final case class Login(
    username: String,
    password: String,
)
object Login {
  implicit val schema: JsonSchema[Login] = JsonSchema.derived
}
