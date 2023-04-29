package harness.archive.model.log

import harness.zio.*
import java.time.OffsetDateTime
import java.util.UUID
import zio.json.*

final case class Log(
    id: UUID,
    appId: UUID,
    logLevel: Option[Logger.LogLevel],
    message: String,
    context: Map[String, String],
    dateTime: OffsetDateTime,
    epochMS: Long,
    keepUntilEpochMS: Long,
)
object Log {
  implicit val jsonCodec: JsonCodec[Log] = DeriveJsonCodec.gen
}
