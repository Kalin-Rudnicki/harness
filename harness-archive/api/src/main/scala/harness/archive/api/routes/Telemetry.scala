package harness.archive.api.routes

import cats.data.NonEmptyList
import cats.syntax.either.*
import harness.archive.api.db.model as M
import harness.archive.api.service.storage.*
import harness.archive.api.util.*
import harness.archive.model as D
import harness.archive.parsers.*
import harness.core.*
import harness.http.server.{given, *}
import harness.sql.*
import harness.sql.query.Transaction
import harness.web.*
import harness.zio.*
import slyce.core.*
import zio.*

object Telemetry {

  val routes: Route[AppStorage & AppTokenStorage & TraceStorage & SessionStorage & Transaction] =
    "telemetry" /: Route.oneOf(
      (HttpMethod.POST / "upload").implement { _ =>
        Transaction.inTransaction {
          for {
            appToken <- SessionUtils.getAppToken

            body <- HttpRequest.jsonBody[D.telemetry.Upload]
            app <- AppStorage.byId(body.appId)
            _ <- HttpResponse.earlyReturn.fromHttpCode.json(HttpCode.Forbidden).unless(app.id == appToken.appId)

            _ <- Logger.log.info(s"Received request to create ${body.traces.length.pluralizeOn("telemetr", "ies", "y")}")
            dbTraces = body.traces.map { trace =>
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
      (HttpMethod.GET / "get-for-app").implement { _ =>
        Transaction.inTransaction {
          for {
            dbUser <- SessionUtils.userFromSessionToken

            appName <- HttpRequest.query.get[String]("app-name")
            app <- Misc.appByName(dbUser, appName)
            dbTraces <- TraceStorage.forAppId(app.id)
          } yield HttpResponse.encodeJson(dbTraces.map(DbToDomain.trace))
        }
      },
      (HttpMethod.GET / "get").implement { _ =>
        Transaction.inTransaction {
          for {
            dbUser <- SessionUtils.userFromSessionToken

            query <- HttpRequest.query.get[String]("query")
            parsedQuery <- QueryParser.parse(Source(query, None)) match {
              case Right(value) => ZIO.succeed(value)
              case Left(errors) => ZIO.fail(HError.UserError(Source.markAll(errors.toList)))
            }
            convertedQuery <- ZIO.eitherNelToUserErrors(ParsedQuery.from(parsedQuery.toExpr).leftMap(NonEmptyList.one))
            dbTraces <- TraceStorage.allForQuery(dbUser.id, convertedQuery.toTraceFilterFunction)
          } yield HttpResponse.encodeJson(dbTraces.map(DbToDomain.trace).sortBy(_.startDateTime).reverse)
        }
      },
    )

}
