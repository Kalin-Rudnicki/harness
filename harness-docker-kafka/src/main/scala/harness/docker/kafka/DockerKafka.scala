package harness.docker.kafka

import harness.docker.*
import harness.kafka.*
import zio.*
import zio.json.*

object DockerKafka {

  final case class Config(
      zookeeper: Config.Zookeeper,
      broker: Config.Broker,
  )
  object Config {

    final case class Zookeeper(
        imageTag: Option[String],
        tickTime: Option[Int],
    )
    object Zookeeper {
      implicit val jsonCodec: JsonCodec[Zookeeper] = DeriveJsonCodec.gen
    }

    final case class Broker(
        imageTag: Option[String],
        brokerId: Int,
    )
    object Broker {
      implicit val jsonCodec: JsonCodec[Broker] = DeriveJsonCodec.gen
    }

    implicit val jsonCodec: JsonCodec[Config] = DeriveJsonCodec.gen

  }

  val containerManager: ContainerManager[KafkaConfig & DockerKafka.Config & DockerAppName] =
    ContainerManager {
      for {
        kafkaConfig <- ZIO.service[KafkaConfig]
        dockerConfig <- ZIO.service[DockerKafka.Config]
        dockerAppName <- DockerAppName.value

        zookeeperName = s"$dockerAppName-zookeeper"
        zookeeperPort = kafkaConfig.zookeeper.withDefaults.port

        brokerName = s"$dockerAppName-kafka-broker"
        brokerPlainTextPort = kafkaConfig.broker.withDefaults.plainTextPort
        brokerPlainTextHostPort = kafkaConfig.broker.withDefaults.plainTextHostPort

        zookeeperContainer =
          DockerContainer
            .init(zookeeperName, "confluentinc/cp-zookeeper")
            .iv(dockerConfig.zookeeper.imageTag)
            .e("ZOOKEEPER_CLIENT_PORT", zookeeperPort.toString)
            .e("ZOOKEEPER_TICK_TIME", dockerConfig.zookeeper.tickTime.getOrElse(2000).toString)
            // .e("ALLOW_ANONYMOUS_LOGIN", yesNo(dockerConfig.zookeeper.allowAnonymousLogin))
            .p(zookeeperPort, zookeeperPort)
        // TODO (KR) : volume?

        brokerContainer =
          DockerContainer
            .init(brokerName, "confluentinc/cp-kafka")
            .iv(dockerConfig.broker.imageTag)
            .e("KAFKA_BROKER_ID", dockerConfig.broker.brokerId.toString)
            .e("KAFKA_LISTENER_SECURITY_PROTOCOL_MAP", "PLAINTEXT:PLAINTEXT,PLAINTEXT_HOST:PLAINTEXT")
            .e("KAFKA_ADVERTISED_LISTENERS", s"PLAINTEXT://$brokerName:$brokerPlainTextPort,PLAINTEXT_HOST://localhost:$brokerPlainTextHostPort")
            .e("KAFKA_ZOOKEEPER_CONNECT", s"$zookeeperName:$zookeeperPort")
            .e("KAFKA_OFFSETS_TOPIC_REPLICATION_FACTOR", 1.toString)
            .e("KAFKA_TRANSACTION_STATE_LOG_MIN_ISR", 1.toString)
            .e("KAFKA_TRANSACTION_STATE_LOG_REPLICATION_FACTOR", 1.toString)
            .e("KAFKA_GROUP_INITIAL_REBALANCE_DELAY_MS", 0.toString)
            // TODO (KR) :
            // .e("ALLOW_PLAINTEXT_LISTENER", yesNo(dockerConfig.broker.allowPlaintextListener))
            // jmx?
            .p(brokerPlainTextPort, brokerPlainTextPort)
            .p(brokerPlainTextHostPort, brokerPlainTextHostPort)
            .d(zookeeperContainer)
        // TODO (KR) : volume?

      } yield zookeeperContainer :: brokerContainer :: Nil
    }

}
