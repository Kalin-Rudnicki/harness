package harness.archive.domain.model

import harness.archive.api.model as Api
import harness.zio.Logger
import java.time.OffsetDateTime

final case class Trace(
    id: Api.telemetry.TraceId,
    appId: Api.app.AppId,
    logLevel: Logger.LogLevel,
    label: String,
    startDateTime: OffsetDateTime,
    endDateTime: OffsetDateTime,
    success: Boolean,
    telemetryContext: Map[String, String],
    logContext: Map[String, String],
    startEpochMS: Long,
    endEpochMS: Long,
    keepUntilEpochMS: Long,
)
