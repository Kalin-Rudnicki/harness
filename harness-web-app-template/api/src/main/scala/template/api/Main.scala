package template.api

import harness.core.*
import harness.docker.DockerNeedsSudo
import harness.docker.sql.DockerPostgres
import harness.http.server.{given, *}
import harness.sql.*
import harness.sql.autoSchema.*
import harness.sql.query.Transaction
import harness.web.*
import harness.zio.*
import template.api.routes as R
import template.api.service.storage.*
import zio.*

object Main extends ExecutableApp {

  type StorageEnv = Transaction & SessionStorage & UserStorage
  type ServerEnv = JDBCConnectionPool
  type ReqEnv = StorageEnv

  // This layer will be evaluated once when the server starts
  val serverLayer: SHRLayer[Scope, ServerEnv] =
    DbConfig.configLayer >>> JDBCConnectionPool.configLayer

  val storageLayer: URLayer[JDBCConnection, StorageEnv] =
    Transaction.liveLayer ++
      SessionStorage.liveLayer ++
      UserStorage.liveLayer

  // This layer will be evaluated for each HTTP request that the server receives
  val reqLayer: SHRLayer[ServerEnv & JDBCConnection & Scope, ReqEnv] =
    storageLayer

  val tables: Tables =
    Tables(
      db.model.User.tableSchema,
      db.model.Session.tableSchema,
    )

  def routes(config: ServerConfig): Route[ServerEnv & ReqEnv] =
    Route.stdRoot(config)(
      R.User.routes,
    )

  override val executable: Executable =
    Executable.fromSubCommands(
      "server" ->
        Executable
          .withLayer[ServerEnv & ServerConfig] {
            serverLayer ++ Config.readLayer[ServerConfig]("http")
          }
          .withEffect {

            MigrationRunner.runMigrationsFromPool(
              Migrations.`0.0.1`,
            ) *>
              ZIO.serviceWithZIO[ServerConfig] { config =>
                Server.start[ServerEnv, ReqEnv](JDBCConnection.poolLayer >>> reqLayer) { routes(config) }
              }
          },
      "docker" ->
        (DockerPostgres.containerManager).toExecutable {
          DbConfig.configLayer ++
            DockerNeedsSudo.configLayer("docker", "needsSudo") ++
            Config.readLayer[DockerPostgres.Config]("docker", "postgres")
        },
    )

}
