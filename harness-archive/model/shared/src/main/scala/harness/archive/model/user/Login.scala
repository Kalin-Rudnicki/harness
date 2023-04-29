package harness.archive.model.user

import zio.json.*

final case class Login(
    username: String,
    password: String,
)
object Login {
  implicit val jsonCodec: JsonCodec[Login] = DeriveJsonCodec.gen
}
