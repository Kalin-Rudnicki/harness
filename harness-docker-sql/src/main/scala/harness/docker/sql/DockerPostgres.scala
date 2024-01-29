package harness.docker.sql

import cats.syntax.option.*
import harness.docker.*
import harness.sql.DbConfig
import harness.zio.*
import zio.*
import zio.json.*

object DockerPostgres {

  final case class Config(
      volumePath: Option[String],
      imageTag: Option[String],
  )
  object Config {
    implicit val jsonCodec: JsonCodec[Config] = DeriveJsonCodec.gen
  }

  val containerManager: ContainerManager[DbConfig & DockerPostgres.Config & DockerAppName] =
    ContainerManager {
      for {
        dbConfig <- ZIO.service[DbConfig]
        dockerConfig <- ZIO.service[DockerPostgres.Config]
        dockerAppName <- DockerAppName.value

        target = dbConfig.target
        containerName = s"$dockerAppName-db"
        containerPort = target.port.getOrElse(5432)

        volumePath <- ZIO.foreach(dockerConfig.volumePath)(Path(_).map(_.absolutePath.show).tap { path => Logger.log.debug(s"volume-path: $path") })

        container =
          DockerContainer
            .init(containerName, "postgres")
            .iv(dockerConfig.imageTag)
            .e("POSTGRES_PASSWORD", dbConfig.credentials.password)
            .e("POSTGRES_DB", target.database)
            .p(containerPort, 5432)
            .v(volumePath, "/var/lib/postgresql/data")
      } yield container :: Nil
    }

}
