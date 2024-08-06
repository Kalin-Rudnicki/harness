package harness.kafka

import harness.zio.*
import org.apache.kafka.clients.consumer.ConsumerRecord
import zio.*
import zio.kafka.consumer.*
import zio.kafka.serde.*

trait HConsumer[K, V] {
  def consume[R: EnvironmentTag](use: ConsumerRecord[K, V] => URIO[R, Unit]): RIO[R & Logger & Telemetry, Unit]
}
object HConsumer {

  // =====| API |=====

  final class ConsumeApplied[K: Tag, V: Tag] {
    def apply[R: EnvironmentTag](use: ConsumerRecord[K, V] => URIO[R, Unit]): RIO[HConsumer[K, V] & R & Logger & Telemetry, Unit] =
      ZIO.serviceWithZIO[HConsumer[K, V]](_.consume(use))
  }

  def consume[K: Tag, V: Tag]: ConsumeApplied[K, V] = new ConsumeApplied[K, V]

  // =====| Live |=====

  def liveLayer[K: Tag, V: Tag](
      keySerde: Serde[Any, K],
      valueSerde: Serde[Any, V],
  ): RLayer[KafkaConfig & ConsumerConfig & Scope, HConsumer[K, V]] =
    ZLayer.fromZIO {
      for {
        kafkaConfig <- ZIO.service[KafkaConfig]
        consumerConfig <- ZIO.service[ConsumerConfig]
        consumer <- Consumer.make(
          ConsumerSettings(List(s"${kafkaConfig.broker.plainTextHost}:${kafkaConfig.broker.withDefaults.plainTextHostPort}"))
            .withGroupId(consumerConfig.groupId)
            .withOffsetRetrieval(Consumer.OffsetRetrieval.Auto(consumerConfig.offsetStrategy)),
        )
      } yield Live(consumer, consumerConfig.topicName, keySerde, valueSerde)
    }

  final case class Live[K, V](
      consumer: Consumer,
      topicName: String,
      keySerde: Serde[Any, K],
      valueSerde: Serde[Any, V],
  ) extends HConsumer[K, V] {

    override def consume[R: EnvironmentTag](use: ConsumerRecord[K, V] => URIO[R, Unit]): RIO[R & Logger & Telemetry, Unit] =
      consumer.consumeWith(Subscription.topics(topicName), keySerde, valueSerde) { record =>
        (for {
          _ <- Logger.log.debug(s"Consuming kafka record: $record")
          _ <- use(record)
        } yield ()).telemetrize.detailed(
          "kafka-consumer",
          "kafka-topic" -> record.topic,
          "kafka-partition" -> record.partition,
          "kafka-offset" -> record.offset,
          "value-class" -> record.value.getClass.getNameWithoutPackage,
        ) @@
          Logger.addContext("record-key" -> record.key).aspect
      }

  }

}
