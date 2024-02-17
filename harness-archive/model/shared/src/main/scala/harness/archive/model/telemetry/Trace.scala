package harness.archive.model.telemetry

import harness.archive.model.app.AppId
import harness.zio.*
import java.time.OffsetDateTime
import zio.json.*

final case class Trace(
    id: TraceId,
    appId: AppId,
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
object Trace {
  implicit val jsonCodec: JsonCodec[Trace] = DeriveJsonCodec.gen
}
