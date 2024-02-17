package harness.web

import harness.zio.*
import zio.json.*

final case class StdClientConfig(
    logTolerance: Logger.LogLevel,
) {
  def basic: HasStdClientConfig.Basic = HasStdClientConfig.Basic(this)
}
object StdClientConfig {
  implicit val jsonCodec: JsonCodec[StdClientConfig] = DeriveJsonCodec.gen
}
