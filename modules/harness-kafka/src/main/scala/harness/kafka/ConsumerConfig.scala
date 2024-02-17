package harness.kafka

import zio.json.*
import zio.kafka.consumer.Consumer

final case class ConsumerConfig(
    groupId: String,
    topicName: String,
    offsetStrategy: Consumer.AutoOffsetStrategy,
)
object ConsumerConfig {

  private val offsetStrategyLowerMap: Map[String, Consumer.AutoOffsetStrategy] =
    List(Consumer.AutoOffsetStrategy.Earliest, Consumer.AutoOffsetStrategy.Latest, Consumer.AutoOffsetStrategy.None).map(s => s.toString.toLowerCase -> s).toMap

  private implicit val offsetStrategyJsonCodec: JsonCodec[Consumer.AutoOffsetStrategy] =
    JsonCodec.string.transformOrFail(s => offsetStrategyLowerMap.get(s.toLowerCase).toRight(s"Invalid AutoOffsetStrategy: $s"), _.toString)

  implicit val jsonCodec: JsonCodec[ConsumerConfig] = DeriveJsonCodec.gen

}
