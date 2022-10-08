package template.api

import harness.core.*
import harness.sql.*
import harness.sql.autoSchema.*
import harness.web.*
import harness.web.server.{given, *}
import harness.zio.*
import template.api.routes as R
import zio.*

object Main extends ExecutableApp {

  override val executable: Executable =
    Executable
      .withParser(ServerConfig.parser)
      .withLayer {
        ZLayer
          .fromZIO { JDBCConnectionPool(ConnectionFactory("jdbc:postgresql:postgres", "kalin", "psql-pass"), 4, 16, Duration.fromSeconds(60)) }
          .mapError(HError.SystemFailure("Error with db-pool", _))
      }
      .withEffectNel { config =>
        PostgresMeta.schemaDiff
          .withPool(Tables(db.model.User.tableSchema, db.model.Session.tableSchema))
          .mapErrorToNel(HError.SystemFailure("Failed to execute schema diff", _)) *>
          Server.start[JDBCConnectionPool, JDBCConnection](
            config,
            JDBCConnection.poolLayer.mapErrorToNel(HError.SystemFailure("Unable to get db connection", _)),
          ) {
            Route.stdRoot(config)(
              R.User.routes,
            )
          }
      }

}
