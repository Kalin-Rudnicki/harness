package harness.archive.api.model.log

import harness.archive.api.model as Api
import harness.zio.*
import java.time.OffsetDateTime
import zio.json.*

final case class Log(
    id: LogId,
    appId: Api.app.AppId,
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
