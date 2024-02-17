package harness.kafka

import zio.json.*

final case class KafkaConfig(
    zookeeper: KafkaConfig.Zookeeper,
    broker: KafkaConfig.Broker,
)
object KafkaConfig {

  final case class Zookeeper(
      port: Option[Int],
  ) { self =>

    object withDefaults {
      def port: Int = self.port.getOrElse(2181)
    }

  }
  object Zookeeper {
    implicit val jsonCodec: JsonCodec[Zookeeper] = DeriveJsonCodec.gen
  }

  final case class Broker(
      plainTextHost: String,
      plainTextPort: Option[Int],
      plainTextHostPort: Option[Int],
  ) { self =>

    object withDefaults {
      def plainTextPort: Int = self.plainTextPort.getOrElse(29092)
      def plainTextHostPort: Int = self.plainTextHostPort.getOrElse(9092)
    }

  }
  object Broker {
    implicit val jsonCodec: JsonCodec[Broker] = DeriveJsonCodec.gen
  }

  implicit val jsonCodec: JsonCodec[KafkaConfig] = DeriveJsonCodec.gen

}
