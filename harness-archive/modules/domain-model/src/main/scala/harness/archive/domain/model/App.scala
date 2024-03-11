package harness.archive.domain.model

import harness.archive.api.model as Api

final case class App(
    id: Api.app.AppId,
    userId: Api.user.UserId,
    name: String,
    logDurationMap: Api.app.DurationMap,
    traceDurationMap: Api.app.DurationMap,
)
