package harness.archive.model.telemetry

import harness.zio.*
import java.time.OffsetDateTime
import java.util.UUID
import zio.json.*

final case class Trace(
    id: UUID,
    appId: UUID,
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
