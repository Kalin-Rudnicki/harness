package template.api

import harness.core.*
import harness.docker.DockerNeedsSudo
import harness.docker.sql.DockerPostgres
import harness.email.*
import harness.http.server.{given, *}
import harness.sql.*
import harness.sql.autoSchema.*
import harness.sql.query.Transaction
import harness.web.*
import harness.zio.*
import template.api.routes as R
import template.api.service.email.*
import template.api.service.storage.*
import zio.*

object Main extends ExecutableApp {

  type StorageEnv = Transaction & SessionStorage & UserStorage
  type ServerEnv = JDBCConnectionPool & EmailService
  type ReqEnv = StorageEnv

  // This layer will be evaluated once when the server starts
  val serverLayer: SHRLayer[Scope, ServerEnv] =
    ZLayer.makeSome[HarnessEnv & Scope, ServerEnv](
      HConfig.readLayer[DbConfig]("db"),
      JDBCConnectionPool.configLayer,
      HConfig.readLayer[EmailConfig]("email", "client"),
      EmailClient.liveLayer,
      HConfig.readLayer[EmailService.Config]("email", "service"),
      EmailService.liveLayer,
    )

  val storageLayer: URLayer[JDBCConnection, StorageEnv] =
    Transaction.liveLayer ++
      SessionStorage.liveLayer ++
      UserStorage.liveLayer

  // This layer will be evaluated for each HTTP request that the server receives
  val reqLayer: SHRLayer[ServerEnv & JDBCConnection & Scope, ReqEnv] =
    storageLayer

  val routes: URIO[ServerConfig, Route[ServerEnv & ReqEnv]] =
    Route.stdRoot(
      R.User.routes,
    )

  private val migrations: PlannedMigrations =
    PlannedMigrations(
      Migrations.`0.0.1`,
      Migrations.`0.0.2`,
      Migrations.`0.0.3`,
    )

  override val executable: Executable =
    Executable.fromSubCommands(
      "server" ->
        Executable
          .withLayer[ServerEnv & ServerConfig] {
            serverLayer ++ HConfig.readLayer[ServerConfig]("http")
          }
          .withEffect {
            MigrationRunner.runMigrationsFromPool(migrations) *>
              routes.flatMap { Server.start[ServerEnv, ReqEnv](JDBCConnection.poolLayer >>> reqLayer) }
          },
      "docker" ->
        (DockerPostgres.containerManager).toExecutable {
          DbConfig.configLayer ++
            DockerNeedsSudo.configLayer("docker", "needsSudo") ++
            HConfig.readLayer[DockerPostgres.Config]("docker", "postgres")
        },
    )

}
