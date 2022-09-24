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
        ZLayer.succeed(ConnectionFactory("jdbc:postgresql:postgres", "kalin", "psql-pass"))
      }
      .withEffectNel { config =>
        PostgresMeta
          .schemaDiff(Tables(db.model.User.tableSchema, db.model.Session.tableSchema))
          .mapErrorToNel(HError.SystemFailure("Failed to execute schema diff", _)) *>
          Server.start(config) {
            Route.stdRoot(config)(
              R.User.routes,
            )
          }
      }

}
