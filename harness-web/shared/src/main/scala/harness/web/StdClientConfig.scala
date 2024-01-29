package harness.web

import harness.core.*
import harness.zio.*
import zio.json.*

final case class StdClientConfig(
    runMode: RunMode,
    logTolerance: Logger.LogLevel,
) {
  def basic: HasStdClientConfig.Basic = HasStdClientConfig.Basic(this)
}
object StdClientConfig {
  implicit val jsonCodec: JsonCodec[StdClientConfig] = {
    implicit val runModeCodec: JsonCodec[RunMode] = JsonCodec.fromHarnessStringEncoderAndDecoder[RunMode]

    DeriveJsonCodec.gen
  }
}
