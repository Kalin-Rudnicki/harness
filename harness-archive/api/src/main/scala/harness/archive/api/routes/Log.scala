package harness.archive.api.routes

import harness.archive.api.db.{model as M, queries as Q}
import harness.archive.model as D
import harness.core.*
import harness.http.server.{given, *}
import harness.sql.*
import harness.sql.query.Transaction
import harness.web.*
import harness.zio.*
import zio.*

object Log {

  val routes: Route[JDBCConnection & Transaction] =
    "log" /: Route.oneOf(
      (HttpMethod.POST / "upload").implement { _ =>
        Transaction.inTransaction {
          for {
            // TODO (KR) :
            _ <- Helpers.warnUserPermissions
            // dbUser <- Helpers.userFromSession

            body <- HttpRequest.jsonBody[Chunk[D.log.Upload]]
            appNameMap <- Helpers.getOrCreateApps(body.map(_.appName).toSet)
            dbLogs = body.map { log =>
              val app = appNameMap(log.appName)
              new M.Log.Identity(
                M.Log.Id.gen,
                app.id,
                log.logLevel,
                log.message,
                log.context,
                log.dateTime,
                log.dateTime.toInstant.toEpochMilli,
                Helpers.keepUntilEpochMillis(app.logDurationMap, log.logLevel, log.dateTime),
              )
            }
            _ <- Q.Log.insert.batched(dbLogs).unit
          } yield HttpResponse.fromHttpCode.Ok
        }
      },
      (HttpMethod.GET / "get").implement { _ =>
        Transaction.inTransaction {
          for {
            // TODO (KR) :
            _ <- Helpers.warnUserPermissions
            // dbUser <- Helpers.userFromSession

            appName <- HttpRequest.query.get[String]("app-name")
            app <- Helpers.appByName(appName)
            dbLogs <- Q.Log.byAppId(app.id).chunk
          } yield HttpResponse.encodeJson(dbLogs.map(Helpers.convert.log))
        }
      },
    )

}
