package harness.archive.model.log

import harness.zio.*
import java.time.OffsetDateTime
import zio.json.*
import harness.archive.model.app.AppId

final case class Log(
    id: LogId,
    appId: AppId,
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
