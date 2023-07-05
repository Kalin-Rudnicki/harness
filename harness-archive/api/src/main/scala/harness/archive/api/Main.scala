package harness.archive.api

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

object Main extends ExecutableApp {

  type StorageEnv = SessionStorage & UserStorage & AppStorage & LogStorage & TraceStorage
  type ServerEnv = StaleDataCleanser & JDBCConnectionPool & Transaction & StorageEnv
  type ReqEnv = JDBCConnection

  private val storageLayer: ULayer[StorageEnv] =
    SessionStorage.liveLayer ++
      UserStorage.liveLayer ++
      AppStorage.liveLayer ++
      LogStorage.liveLayer ++
      TraceStorage.liveLayer

  // This layer will be evaluated once when the server starts
  val serverLayer: SHRLayer[Scope, ServerEnv] =
    ZLayer.fromZIO { JDBCConnectionPool(ConnectionFactory("jdbc:postgresql:archive", "kalin", "psql-pass"), 4, 16, Duration.fromSeconds(60)) } ++
      ZLayer.succeed(Transaction.Live) ++
      storageLayer ++
      StaleDataCleanser.live(1.minute, 1.minute, 1.minute, 5.minutes, 15.minutes)

  // This layer will be evaluated for each HTTP request that the server receives
  val reqLayer: SHRLayer[ServerEnv & Scope, ReqEnv] =
    JDBCConnection.poolLayer

  val tables: Tables =
    Tables(
      M.User.tableSchema,
      M.Session.tableSchema,
      M.App.tableSchema,
      M.Log.tableSchema,
      M.Trace.tableSchema,
    )

  def routes(config: ServerConfig): Route[ServerEnv & ReqEnv] =
    Route.stdRoot(config)(
      R.User.routes,
      R.Log.routes,
      R.Telemetry.routes,
    )

  private val schemaDiff: HRIO[JDBCConnectionPool & Logger & Telemetry, Unit] =
    PostgresMeta.schemaDiff
      .withPool(tables)
      .mapError(HError.SystemFailure("Failed to execute schema diff", _))

  override val executable: Executable =
    Executable
      .withParser(ServerConfig.parser)
      .withLayer[ServerEnv](serverLayer)
      .withEffect { config =>
        schemaDiff *>
          StaleDataCleanser.startFiber *>
          Server.start[ServerEnv, ReqEnv](config, reqLayer) { routes(config) }
      }

}
