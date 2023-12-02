package harness.docker

import cats.data.NonEmptyList
import cats.syntax.either.*
import cats.syntax.parallel.*
import cats.syntax.traverse.*
import harness.core.*
import harness.zio.*
import java.time.ZonedDateTime
import java.time.format.DateTimeFormatter
import scala.util.Try
import zio.*
import zio.json.*

object DockerCommands {

  private val portMappingRegex = ":::(\\d+)->(\\d+)/[a-z]+".r

  final case class Port(PublicPort: Int, PrivatePort: Int) {
    override def toString: String = s"$PublicPort->$PrivatePort"
  }
  implicit val portsDecoder: JsonDecoder[List[Port]] =
    JsonDecoder.string.mapOrFail {
      _.split(", ").toList.filter(_.nonEmpty).grouped(2).toList.traverse {
        case List(_, portMappingRegex(publicPort, privatePort)) => Port(publicPort.toInt, privatePort.toInt).asRight
        case list                                               => s"Malformed list: $list".asLeft
      }
    }

  private implicit val formatter: DateTimeFormatter = DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss Z z")
  implicit val zdtDecoder: JsonDecoder[ZonedDateTime] =
    JsonDecoder.string.mapOrFail { string =>
      Try { ZonedDateTime.parse(string, formatter) }.toEither.leftMap(_.getMessage)
    }

  final case class PsContainer(
      ID: String,
      Names: String, // TODO (KR) :
      Image: String,
      Command: String,
      CreatedAt: ZonedDateTime,
      Ports: List[Port],
      // Labels: Map[String, String],
      State: String,
      Status: String,
  )
  object PsContainer {
    implicit val jsonCodec: JsonDecoder[PsContainer] = DeriveJsonDecoder.gen[PsContainer]
  }

  val getContainers: HRIO[DockerNeedsSudo & Logger, List[PsContainer]] =
    for {
      _ <- Logger.log.info("Getting docker containers")
      dockerNeedsSudo <- DockerNeedsSudo.value
      dockerPsCommand = Sys.Command("docker", "ps", "-a", "--format", "{{json .}}").sudoIf(dockerNeedsSudo)
      sysString <- Sys.executeString0(dockerPsCommand).map(_.replace("\\u003e", ">"))
      containers <- ZIO.eitherNelToInternalDefects(sysString.split('\n').toList.filter(_.nonEmpty).parTraverse(_.fromJson[PsContainer].toEitherNel))
      _ <- Logger.log.debug(s"Containers:${containers.map { c => s"\n  - ${c.Names} (${c.State}) : ${c.Ports.mkString(", ")}" }.mkString}")
    } yield containers

  def stopAndRemoveContainer(container: PsContainer): HRIO[DockerNeedsSudo & Logger, Unit] =
    DockerNeedsSudo.value.flatMap { dockerNeedsSudo =>
      val stop =
        Logger.log.important(s"Stopping container '${container.Names}'") *>
          Sys.executeString0(Sys.Command("docker", "stop", container.Names).sudoIf(dockerNeedsSudo))
      val rm =
        Logger.log.important(s"Removing container '${container.Names}'") *>
          Sys.executeString0(Sys.Command("docker", "rm", container.Names).sudoIf(dockerNeedsSudo)).unit

      Logger.log.info(s"Found existing container '${container.Names}'") *>
        (container.State match {
          case "running" => stop *> rm
          case "exited"  => rm
          case state     => ZIO.fail(HError.SystemFailure(s"Unable to act on unknown state '$state' for container '${container.Names}'"))
        })
    }

  def runContainer(container: DockerContainer): HRIO[DockerNeedsSudo & Logger, Unit] =
    for {
      _ <- Logger.log.info(s"Running docker container '${container.name}'")
      dockerNeedsSudo <- DockerNeedsSudo.value
      cmd = Sys.Command(
        NonEmptyList(
          "docker",
          List(
            List("run", "--name", container.name),
            container.envVars.flatMap(_.toArgs),
            container.portMappings.flatMap(_.toArgs("-p")),
            container.volumeMappings.flatMap(_.toArgs("-v")),
            List("-d", container.image),
          ).flatten,
        ),
      )
      _ <- Sys.executeString0(cmd.sudoIf(dockerNeedsSudo))
    } yield ()

}
