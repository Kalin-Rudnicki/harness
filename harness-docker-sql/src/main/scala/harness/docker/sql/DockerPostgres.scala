package harness.docker.sql

import cats.syntax.option.*
import harness.core.HError
import harness.docker.*
import harness.sql.DbConfig
import harness.zio.*
import zio.*
import zio.json.*

object DockerPostgres {

  final case class Config(
      volumePath: Option[String],
  )
  object Config {
    implicit val jsonCodec: JsonCodec[Config] = DeriveJsonCodec.gen
  }

  val containerManager: ContainerManager[DbConfig & DockerPostgres.Config] =
    ContainerManager {
      for {
        dbConfig <- ZIO.service[DbConfig]
        postgresConfig <- ZIO.service[DockerPostgres.Config]

        target = dbConfig.target
        containerName = s"postgres-${target.database}"
        containerPort = target.port.getOrElse(5432)

        volumePath <- ZIO.foreach(postgresConfig.volumePath)(Path(_).map(_.absolutePath.show).tap { path => Logger.log.debug(s"volume-path: $path") })

        container =
          DockerContainer
            .init(containerName, "postgres")
            .e("POSTGRES_PASSWORD", dbConfig.credentials.password)
            .e("POSTGRES_DB", target.database)
            .p(containerPort, 5432)
            .v(volumePath, "/var/lib/postgresql/data")
      } yield container :: Nil
    }

}
