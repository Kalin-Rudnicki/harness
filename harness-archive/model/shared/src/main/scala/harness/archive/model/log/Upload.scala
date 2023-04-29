package harness.archive.model.log

import harness.zio.*
import java.time.OffsetDateTime
import zio.json.*

final case class Upload(
    appName: String,
    logLevel: Option[Logger.LogLevel],
    message: String,
    context: Map[String, String],
    dateTime: OffsetDateTime,
)
object Upload {
  implicit val jsonCodec: JsonCodec[Upload] = DeriveJsonCodec.gen
}
