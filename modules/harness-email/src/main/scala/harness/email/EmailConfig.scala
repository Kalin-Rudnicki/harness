package harness.email

import harness.core.Enum
import harness.zio.*
import harness.zio.json.*
import zio.json.*

final case class EmailConfig(
    host: String,
    port: Int,
    authType: EmailConfig.AuthType,
    passwordMap: Option[Map[EmailAddress, String]],
) derives JsonCodec
object EmailConfig {

  enum AuthType extends Enum[AuthType] { case NoAuth, TLS, SSL }
  object AuthType extends Enum.Companion[AuthType] {
    implicit val jsonCodec: JsonCodec[AuthType] = JsonCodec.`enum`[AuthType, String]
  }

}
