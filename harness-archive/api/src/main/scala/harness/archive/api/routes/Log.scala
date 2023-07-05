package harness.archive.api.routes

import harness.archive.api.db.{model as M, queries as Q}
import harness.archive.api.service.storage.*
import harness.archive.api.util.*
import harness.archive.model as D
import harness.core.*
import harness.http.server.{given, *}
import harness.sql.*
import harness.sql.query.Transaction
import harness.web.*
import harness.zio.*
import zio.*

object Log {

  val routes: Route[AppStorage & LogStorage & JDBCConnection & Transaction] =
    "log" /: Route.oneOf(
      (HttpMethod.POST / "upload").implement { _ =>
        Transaction.inTransaction {
          for {
            // TODO (KR) :
            _ <- Misc.warnUserPermissions
            // dbUser <- SessionUtils.userFromSession

            body <- HttpRequest.jsonBody[Chunk[D.log.Upload]]
            appNameMap <- Misc.getOrCreateApps(body.map(_.appName).toSet)
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
                Misc.keepUntilEpochMillis(app.logDurationMap, log.logLevel, log.dateTime),
              )
            }
            _ <- LogStorage.insertAll(dbLogs)
          } yield HttpResponse.fromHttpCode.Ok
        }
      },
      (HttpMethod.GET / "get").implement { _ =>
        Transaction.inTransaction {
          for {
            // TODO (KR) :
            _ <- Misc.warnUserPermissions
            // dbUser <- SessionUtils.userFromSession

            appName <- HttpRequest.query.get[String]("app-name")
            app <- Misc.appByName(appName)
            dbLogs <- LogStorage.byAppId(app.id)
          } yield HttpResponse.encodeJson(dbLogs.map(DbToDomain.log))
        }
      },
    )

}
