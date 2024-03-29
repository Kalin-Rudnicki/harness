package harness.archive.client

import harness.archive.model.app.AppId
import harness.zio.*
import java.time.Duration
import zio.json.*

final case class ArchiveConfig(
    logTolerance: Logger.LogLevel,
    appId: AppId,
    appToken: String,
    baseUrl: String,
    queueChunkSize: Int,
    queueDumpEvery: Option[Duration],
)
object ArchiveConfig {
  implicit val jsonCodec: JsonCodec[ArchiveConfig] = DeriveJsonCodec.gen
}
