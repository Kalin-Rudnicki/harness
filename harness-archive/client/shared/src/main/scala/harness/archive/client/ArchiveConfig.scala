package harness.archive.client

import harness.zio.*
import zio.json.*

final case class ArchiveConfig(
    logTolerance: Logger.LogLevel,
    appName: String,
    baseUrl: String,
)
object ArchiveConfig {
  implicit val jsonCodec: JsonCodec[ArchiveConfig] = DeriveJsonCodec.gen
}
