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

object Telemetry {

  val routes: Route[JDBCConnection & Transaction] =
    "telemetry" /: Route.oneOf(
      (HttpMethod.POST / "upload").implement { _ =>
        Transaction.inTransaction {
          for {
            // TODO (KR) :
            _ <- Helpers.warnUserPermissions
            // dbUser <- Helpers.userFromSession

            body <- HttpRequest.jsonBody[Chunk[D.telemetry.Upload]]
            appNameMap <- Helpers.getOrCreateApps(body.map(_.appName).toSet)
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
                Helpers.keepUntilEpochMillis(app.traceDurationMap, trace.logLevel, trace.endDateTime),
              )
            }
            _ <- Q.Trace.insert.batched(dbTraces).unit
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
            dbTraces <- Q.Trace.byAppId(app.id).chunk
          } yield HttpResponse.encodeJson(dbTraces.map(Helpers.convert.trace))
        }
      },
    )

}
