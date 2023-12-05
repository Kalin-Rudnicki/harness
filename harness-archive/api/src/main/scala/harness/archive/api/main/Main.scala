package harness.archive.api.main

import cats.data.NonEmptyList
import harness.archive.api.db.model as M
import harness.archive.api.routes as R
import harness.archive.api.service.*
import harness.archive.api.service.storage.*
import harness.core.*
import harness.docker.*
import harness.docker.sql.DockerPostgres
import harness.http.server.{given, *}
import harness.sql.*
import harness.sql.autoSchema.*
import harness.sql.query.Transaction
import harness.web.*
import harness.zio.*
import zio.*

object Main extends ExecutableApp {

  override val executable: Executable =
    Executable.fromSubCommands(
      "server" -> ServerMain.executable,
      "docker" ->
        (DockerPostgres.containerManager).toExecutable {
          DbConfig.configLayer ++
            DockerNeedsSudo.configLayer("docker", "needsSudo") ++
            Config.readLayer[DockerPostgres.Config]("docker", "postgres")
        },
      "stale-data-cleanser" -> StaleDataCleanserMain.executable,
    )

}
