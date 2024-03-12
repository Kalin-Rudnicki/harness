package harness.archive.domain.model

import harness.archive.api.model as Api
import java.time.OffsetDateTime
import java.util.UUID

final case class AppToken(
    id: Api.app.AppTokenId,
    appId: Api.app.AppId,
    name: String,
    createdAt: OffsetDateTime,
    token: Api.app.AppToken,
)
object AppToken {

  def forApp(app: App, tokenName: String, now: OffsetDateTime): AppToken =
    AppToken(
      id = Api.app.AppTokenId.gen,
      appId = app.id,
      name = tokenName,
      createdAt = now,
      token = Api.app.AppToken(s"${UUID.randomUUID()}:${UUID.randomUUID()}"),
    )

}
