package harness.archive.domain.model

import harness.archive.api.model as Api
import java.time.OffsetDateTime

final case class AppToken(
    id: Api.app.AppTokenId,
    appId: Api.app.AppId,
    name: String,
    createdAt: OffsetDateTime,
    token: Api.app.AppToken,
)
