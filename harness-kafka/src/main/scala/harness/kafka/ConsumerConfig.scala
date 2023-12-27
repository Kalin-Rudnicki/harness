package harness.kafka

import zio.json.*

final case class ConsumerConfig(
    groupId: String,
    topicName: String,
)
object ConsumerConfig {
  implicit val jsonCodec: JsonCodec[ConsumerConfig] = DeriveJsonCodec.gen
}
