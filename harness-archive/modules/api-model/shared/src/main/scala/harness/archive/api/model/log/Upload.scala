package harness.archive.api.model.log

import harness.archive.api.model as Api
import harness.zio.*
import java.time.OffsetDateTime
import zio.Chunk
import zio.json.*

final case class Upload(
    logs: Chunk[Upload.Log],
)
object Upload {

  final case class Log(
      logLevel: Option[Logger.LogLevel],
      message: String,
      context: Map[String, String],
      dateTime: OffsetDateTime,
  )
  object Log {
    implicit val jsonCodec: JsonCodec[Log] = DeriveJsonCodec.gen
  }

  implicit val jsonCodec: JsonCodec[Upload] = DeriveJsonCodec.gen

}
