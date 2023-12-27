package harness.docker.kafka

import harness.cli.*
import harness.core.*
import harness.docker.*
import harness.kafka.*
import harness.zio.*
import java.util.UUID
import org.apache.kafka.clients.producer.ProducerRecord
import zio.*
import zio.json.*
import zio.kafka.consumer.*
import zio.kafka.producer.*
import zio.kafka.serde.Serde

object TestMain extends ExecutableApp {

  final case class Team(name: String, score: Int)
  object Team {
    implicit val jsonCodec: JsonCodec[Team] = DeriveJsonCodec.gen
  }

  final case class Match(id: UUID, home: Team, away: Team)
  object Match {
    implicit val jsonCodec: JsonCodec[Match] = DeriveJsonCodec.gen
  }

  private implicit val keySerde: Serde[Any, UUID] = Serde.uuid
  private implicit val valueSerde: Serde[Any, Match] = HSerdes.jsonEncoded[Match]

  override val executable: Executable =
    Executable.fromSubCommands(
      "consumer" ->
        Executable
          .withLayer {
            ZLayer.makeSome[HConfig & Scope, HConsumer[UUID, Match]](
              HConfig.readLayer[KafkaConfig]("kafka"),
              HConfig.readLayer[ConsumerConfig]("kafka", "consumer"),
              HConsumer.liveLayer(keySerde, valueSerde),
            )
          }
          .withEffect {
            for {
              _ <- Logger.log.info("Harness Kafka TestMain - consumer")
              _ <- HConsumer.consume[UUID, Match] { record =>
                Logger.log.info(s"record: ${record.key} / ${record.value}")
              }
              _ <- Logger.log.info("consumption ended")
            } yield ()
          },
      "producer" ->
        Executable
          .withParser {
            Parser.value[String](LongName.unsafe("home-team-name")) &&
            Parser.value[Int](LongName.unsafe("home-team-score")) &&
            Parser.value[String](LongName.unsafe("away-team-name")) &&
            Parser.value[Int](LongName.unsafe("away-team-score"))
          }
          .withLayer {
            ZLayer.makeSome[HConfig & Scope, HProducer[Match]](
              HConfig.readLayer[KafkaConfig]("kafka"),
              HConfig.readLayer[ProducerConfig]("kafka", "producer"),
              HProducer.liveLayer(keySerde, valueSerde, _.id),
            )
          }
          .withEffect { case (homeTeamName, homeTeamScore, awayTeamName, awayTeamScore) =>
            for {
              _ <- Logger.log.info("Harness Kafka TestMain - producer")
              id <- Random.nextUUID
              _ <- HProducer.produce(Match(id, Team(homeTeamName, homeTeamScore), Team(awayTeamName, awayTeamScore)))
            } yield ()
          },
      "docker" ->
        (DockerKafka.containerManager).toExecutable {
          DockerNeedsSudo.configLayer("docker", "needsSudo") ++
            DockerAppName.configLayer("docker", "appName") ++
            HConfig.readLayer[KafkaConfig]("kafka") ++
            HConfig.readLayer[DockerKafka.Config]("docker", "kafka")
        },
    )

}
