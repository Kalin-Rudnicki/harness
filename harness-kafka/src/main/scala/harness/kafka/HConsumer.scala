package harness.kafka

import harness.core.*
import harness.zio.*
import org.apache.kafka.clients.consumer.ConsumerRecord
import zio.*
import zio.kafka.consumer.*
import zio.kafka.serde.*

trait HConsumer[K, V] {
  def consume[R: EnvironmentTag](use: ConsumerRecord[K, V] => URIO[R, Unit]): HRIO[R & Logger & Telemetry, Unit]
}
object HConsumer {

  // =====| API |=====

  final class ConsumeApplied[K: Tag, V: Tag] {
    def apply[R: EnvironmentTag](use: ConsumerRecord[K, V] => URIO[R, Unit]): HRIO[HConsumer[K, V] & R & Logger & Telemetry, Unit] =
      ZIO.serviceWithZIO[HConsumer[K, V]](_.consume(use))
  }

  def consume[K: Tag, V: Tag]: ConsumeApplied[K, V] = new ConsumeApplied[K, V]

  // =====| Live |=====

  def liveLayer[K: Tag, V: Tag](
      keySerde: Serde[Any, K],
      valueSerde: Serde[Any, V],
  ): HRLayer[KafkaConfig & ConsumerConfig & Scope, HConsumer[K, V]] =
    ZLayer.fromZIO {
      for {
        kafkaConfig <- ZIO.service[KafkaConfig]
        consumerConfig <- ZIO.service[ConsumerConfig]
        consumer <-
          Consumer
            .make(
              // TODO (KR) : hardcoded to localhost? no-no?
              ConsumerSettings(List(s"localhost:${kafkaConfig.broker.withDefaults.plainTextHostPort}"))
                .withGroupId(consumerConfig.groupId)
                .withOffsetRetrieval(Consumer.OffsetRetrieval.Auto(Consumer.AutoOffsetStrategy.Earliest)),
            )
            .mapError(HError.SystemFailure("Unable to create kafka consumer", _))
      } yield Live(consumer, consumerConfig.topicName, keySerde, valueSerde)
    }

  final case class Live[K, V](
      consumer: Consumer,
      topicName: String,
      keySerde: Serde[Any, K],
      valueSerde: Serde[Any, V],
  ) extends HConsumer[K, V] {

    override def consume[R: EnvironmentTag](use: ConsumerRecord[K, V] => URIO[R, Unit]): HRIO[R & Logger & Telemetry, Unit] =
      consumer
        .consumeWith(Subscription.topics(topicName), keySerde, valueSerde) { record =>
          Logger.addContext("record-key" -> record.key) {
            (for {
              _ <- Logger.log.debug(s"Consuming kafka record: $record")
              _ <- use(record)
            } yield ()).telemetrize(
              "kafka-consumer",
              "kafka-topic" -> record.topic,
              "kafka-partition" -> record.partition,
              "kafka-offset" -> record.offset,
              "value-class" -> record.value.getClass.getNameWithoutPackage,
            )
          }
        }
        .mapError(HError.SystemFailure("Error with kafka consumer", _))

  }

}
