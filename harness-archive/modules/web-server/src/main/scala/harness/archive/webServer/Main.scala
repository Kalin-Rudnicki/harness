package harness.archive.webServer

import harness.archive.domain.impl.storage.postgres.*
import harness.archive.webServer.api.*
import harness.archive.webServer.route.*
import harness.http.server.*
import harness.http.server.ServerConfig
import harness.sql.*
import harness.sql.autoSchema.*
import harness.sql.query.Transaction
import harness.web.*
import harness.zio.*
import zio.*
import zio.json.*

object Main extends ExecutableApp {

  type ServerEnv = JDBCConnectionPool & StdClientConfig & SessionConfig
  type ReqEnv = UserApi & Transaction

  val serverLayer: RLayer[HarnessEnv & Scope, ServerEnv] =
    ZLayer.makeSome[HarnessEnv & Scope, ServerEnv](
      HConfig.readLayer[DbConfig]("db"),
      JDBCConnectionPool.configLayer,
      HConfig.readLayer[StdClientConfig]("ui"),
      HConfig.readLayer[SessionConfig]("http", "session"),
    )

  val reqLayer: URLayer[ServerEnv & JDBCConnection & Scope, ReqEnv] =
    ZLayer.makeSome[ServerEnv & JDBCConnection & Scope, ReqEnv](
      // db
      Transaction.liveLayer,
      LiveUserStorage.liveLayer,
      LiveSessionStorage.liveLayer,
      // api
      UserApi.layer,
    )

  val routes: URIO[StdClientConfig & ServerConfig, Route[ServerEnv & ReqEnv]] =
    for {
      uiConfig <- ZIO.service[StdClientConfig]
      config = uiConfig.basic
      r <- Route.stdRoot(config)(
        UserRoutes.routes,
      )
    } yield r

  private val migrations: PlannedMigrations =
    PlannedMigrations(
      Migrations.`0.0.1`,
      Migrations.`0.0.2`,
      Migrations.`0.0.3`,
      Migrations.`0.0.4`,
    )

  override val executable: Executable =
    Executable
      .withLayer[ServerEnv & ServerConfig, Throwable] {
        serverLayer ++ HConfig.readLayer[ServerConfig]("http")
      }
      .withEffectSimple {
        MigrationRunner.runMigrationsFromPool(migrations) *>
          routes.flatMap { Server.start[ServerEnv, ReqEnv](JDBCConnection.poolLayer >>> reqLayer) }
      }

}
