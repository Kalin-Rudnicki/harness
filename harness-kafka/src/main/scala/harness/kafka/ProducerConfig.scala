package harness.kafka

import zio.json.*

final case class ProducerConfig(
    topicName: String,
)
object ProducerConfig {
  implicit val jsonCodec: JsonCodec[ProducerConfig] = DeriveJsonCodec.gen
}
