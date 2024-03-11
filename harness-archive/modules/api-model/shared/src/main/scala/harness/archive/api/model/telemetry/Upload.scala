package harness.archive.api.model.telemetry

import harness.archive.api.model as Api
import harness.zio.*
import java.time.OffsetDateTime
import zio.Chunk
import zio.json.*

final case class Upload(
    appId: Api.app.AppId,
    traces: Chunk[Upload.Trace],
)
object Upload {

  final case class Trace(
      logLevel: Logger.LogLevel,
      label: String,
      startDateTime: OffsetDateTime,
      endDateTime: OffsetDateTime,
      success: Boolean,
      telemetryContext: Map[String, String],
      logContext: Map[String, String],
  )
  object Trace {
    implicit val jsonCodec: JsonCodec[Trace] = DeriveJsonCodec.gen
  }

  implicit val jsonCodec: JsonCodec[Upload] = DeriveJsonCodec.gen

}
