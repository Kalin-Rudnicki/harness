package harness.web

import harness.zio.*
import zio.json.*

final case class StdClientConfig(
    logLevel: Logger.LogLevel,
) derives JsonCodec {
  def basic: HasStdClientConfig.Basic = HasStdClientConfig.Basic(this)
}
