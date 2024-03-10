package harness.archive.api.model.config

import harness.web.*
import zio.json.*

final case class UiConfig(
    stdClientConfig: StdClientConfig,
    stripePublishableKey: String,
) extends HasStdClientConfig
object UiConfig {
  implicit val jsonCodec: JsonCodec[UiConfig] = DeriveJsonCodec.gen
}
