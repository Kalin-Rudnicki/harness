package harness.kafka

import harness.zio.*
import org.apache.kafka.clients.producer.{ProducerRecord, RecordMetadata}
import zio.*
import zio.kafka.producer.*
import zio.kafka.serde.*

trait HProducer[V] {
  def produce(value: V): RIO[Logger & Telemetry, RecordMetadata]
}
object HProducer {

  // =====| API |=====

  def produce[V: Tag](value: V): RIO[HProducer[V] & Logger & Telemetry, RecordMetadata] =
    ZIO.serviceWithZIO[HProducer[V]](_.produce(value))

  // =====| Live |=====

  def liveLayer[K: Tag, V: Tag](
      keySerde: Serde[Any, K],
      valueSerde: Serde[Any, V],
      keyFromValue: V => K,
      partition: K => Option[Int] = (_: K) => None,
  ): RLayer[KafkaConfig & ProducerConfig & Scope, HProducer[V]] =
    ZLayer.fromZIO {
      for {
        kafkaConfig <- ZIO.service[KafkaConfig]
        producerConfig <- ZIO.service[ProducerConfig]
        producer <- Producer.make(
          ProducerSettings(List(s"${kafkaConfig.broker.plainTextHost}:${kafkaConfig.broker.withDefaults.plainTextHostPort}")),
        )
      } yield Live(producer, producerConfig.topicName, keySerde, valueSerde, keyFromValue, partition)
    }

  final case class Live[K, V](
      producer: Producer,
      topicName: String,
      keySerde: Serde[Any, K],
      valueSerde: Serde[Any, V],
      keyFromValue: V => K,
      partition: K => Option[Int],
  ) extends HProducer[V] {

    override def produce(value: V): RIO[Logger & Telemetry, RecordMetadata] = {
      val key = keyFromValue(value)
      val record = partition(key) match {
        case Some(partition) => new ProducerRecord(topicName, partition, key, value)
        case None            => new ProducerRecord(topicName, key, value)
      }
      Logger.addContext("record-key" -> record.key) {
        (for {
          _ <- Logger.log.debug(s"Producing kafka record: $record")
          res <- producer.produce(record, keySerde, valueSerde)
        } yield res).telemetrize(
          "kafka-producer",
          "kafka-topic" -> record.topic,
          "kafka-partition" -> record.partition,
          "value-class" -> record.value.getClass.getNameWithoutPackage,
        )
      }
    }

  }

}
