package harness.archive.webServer.api

import zio.json.*

final case class SessionConfig(
    key: String,
    isSecure: Boolean,
)
object SessionConfig {
  implicit val jsonCodec: JsonCodec[SessionConfig] = DeriveJsonCodec.gen
}
