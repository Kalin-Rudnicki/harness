package harness.archive.api.main

import harness.docker.*
import harness.docker.sql.DockerPostgres
import harness.sql.*
import harness.sql.autoSchema.MigrationRunner
import harness.zio.*
import zio.*

object Main extends ExecutableApp {

  override val executable: Executable =
    Executable.fromSubCommands(
      "server" -> ServerMain.executable,
      "db-rollback" -> MigrationRunner.rollbackExecutable(ServerMain.serverLayer),
      "docker" ->
        (DockerPostgres.containerManager).toExecutable {
          DbConfig.configLayer ++
            DockerNeedsSudo.configLayer("docker", "needsSudo") ++
            DockerAppName.configLayer("docker", "appName") ++
            HConfig.readLayer[DockerPostgres.Config]("docker", "postgres")
        },
      "stale-data-cleanser" -> StaleDataCleanserMain.executable,
    )

}
