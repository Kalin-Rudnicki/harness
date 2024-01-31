package harness.archive.model.app

import java.time.OffsetDateTime
import zio.json.*

final case class AppToken(
    id: AppTokenId,
    appId: AppId,
    name: String,
    createdAt: OffsetDateTime,
)
object AppToken {
  implicit val jsonCodec: JsonCodec[AppToken] = DeriveJsonCodec.gen
}
