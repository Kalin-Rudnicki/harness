package harness.archive.api.main

import cats.data.NonEmptyList
import harness.archive.api.db.model as M
import harness.archive.api.routes as R
import harness.archive.api.service.*
import harness.archive.api.service.storage.*
import harness.core.*
import harness.http.server.{given, *}
import harness.sql.*
import harness.sql.autoSchema.*
import harness.sql.query.Transaction
import harness.web.*
import harness.zio.*
import zio.*

object ServerMain {

  type StorageEnv = Transaction & SessionStorage & UserStorage & AppStorage & LogStorage & TraceStorage
  type ServerEnv = JDBCConnectionPool
  type ReqEnv = StorageEnv

  // This layer will be evaluated once when the server starts
  val serverLayer: SHRLayer[Scope, ServerEnv] =
    Shared.poolLayer

  val storageLayer: URLayer[JDBCConnection, StorageEnv] =
    Transaction.liveLayer ++
      SessionStorage.liveLayer ++
      UserStorage.liveLayer ++
      AppStorage.liveLayer ++
      LogStorage.liveLayer ++
      TraceStorage.liveLayer

  // This layer will be evaluated for each HTTP request that the server receives
  val reqLayer: SHRLayer[ServerEnv & JDBCConnection & Scope, ReqEnv] =
    storageLayer

  def routes(config: ServerConfig): Route[ServerEnv & ReqEnv] =
    Route.stdRoot(config)(
      R.User.routes,
      R.Log.routes,
      R.Telemetry.routes,
    )

  val executable: Executable =
    Executable
      .withParser(ServerConfig.parser)
      .withLayer[ServerEnv](serverLayer)
      .withEffect { config =>
        MigrationRunner.runMigrationsFromPool(
          Migrations.`0.0.1`,
          Migrations.`0.0.2`,
        ) *>
          Server.start[ServerEnv, ReqEnv](config, JDBCConnection.poolLayer >>> reqLayer) { routes(config) }
      }

}
