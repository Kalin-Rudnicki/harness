package harness.archive.api.routes

import harness.archive.api.db.model as M
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

object Telemetry {

  val routes: Route[AppStorage & TraceStorage & JDBCConnection & Transaction] =
    "telemetry" /: Route.oneOf(
      (HttpMethod.POST / "upload").implement { _ =>
        Transaction.inTransaction {
          for {
            // TODO (KR) :
            _ <- Misc.warnUserPermissions
            // dbUser <- SessionUtils.userFromSession

            body <- HttpRequest.jsonBody[Chunk[D.telemetry.Upload]]
            appNameMap <- Misc.getOrCreateApps(body.map(_.appName).toSet)
            dbTraces = body.map { trace =>
              val app = appNameMap(trace.appName)
              new M.Trace.Identity(
                M.Trace.Id.gen,
                app.id,
                trace.logLevel,
                trace.label,
                trace.startDateTime,
                trace.endDateTime,
                trace.success,
                trace.telemetryContext,
                trace.logContext,
                trace.startDateTime.toInstant.toEpochMilli,
                trace.endDateTime.toInstant.toEpochMilli,
                Misc.keepUntilEpochMillis(app.traceDurationMap, trace.logLevel, trace.endDateTime),
              )
            }
            _ <- TraceStorage.insertAll(dbTraces)
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
            dbTraces <- TraceStorage.byAppId(app.id)
          } yield HttpResponse.encodeJson(dbTraces.map(DbToDomain.trace))
        }
      },
    )

}
