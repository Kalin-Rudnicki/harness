package template.api

import harness.core.*
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
    Config.layer.jarResource("application.conf.json") >>> DbConfig.configLayer >>> JDBCConnectionPool.configLayer

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
    Executable
      .withParser(ServerConfig.parser)
      .withLayer[ServerEnv](serverLayer)
      .withEffect { config =>
        PostgresMeta.schemaDiff
          .withPool(tables)
          .mapError(HError.SystemFailure("Failed to execute schema diff", _)) *>
          Server.start[ServerEnv, ReqEnv](config, JDBCConnection.poolLayer >>> reqLayer) { routes(config) }
      }

}
