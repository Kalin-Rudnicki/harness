package harness.archive.domain.model

import harness.archive.api.model as Api
import harness.zio.Logger
import java.time.OffsetDateTime

final case class Log(
    id: Api.log.LogId,
    appId: Api.app.AppId,
    logLevel: Option[Logger.LogLevel],
    message: String,
    context: Map[String, String],
    dateTime: OffsetDateTime,
    epochMS: Long,
    keepUntilEpochMS: Long,
)
