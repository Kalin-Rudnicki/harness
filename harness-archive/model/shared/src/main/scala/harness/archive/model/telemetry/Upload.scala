package harness.archive.model.telemetry

import harness.zio.*
import java.time.OffsetDateTime
import zio.json.*

final case class Upload(
    appName: String,
    logLevel: Logger.LogLevel,
    label: String,
    startDateTime: OffsetDateTime,
    endDateTime: OffsetDateTime,
    success: Boolean,
    telemetryContext: Map[String, String],
    logContext: Map[String, String],
)
object Upload {
  implicit val jsonCodec: JsonCodec[Upload] = DeriveJsonCodec.gen
}
