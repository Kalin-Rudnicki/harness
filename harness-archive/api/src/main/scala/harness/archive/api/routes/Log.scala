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

object Log {

  val routes: Route[AppStorage & AppTokenStorage & LogStorage & SessionStorage & Transaction] =
    "log" /: Route.oneOf(
      (HttpMethod.POST / "upload").implement { _ =>
        Transaction.inTransaction {
          for {
            appToken <- SessionUtils.getAppToken

            body <- HttpRequest.jsonBody[D.log.Upload]
            app <- AppStorage.byId(body.appId)
            _ <- HttpResponse.earlyReturn.fromHttpCode.json(HttpCode.Forbidden).unless(app.id == appToken.appId)

            _ <- Logger.log.info(s"Received request to create ${body.logs.length.pluralizeOn("log")}")
            dbLogs = body.logs.map { log =>
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
      (HttpMethod.GET / "get-for-app").implement { _ =>
        Transaction.inTransaction {
          for {
            dbUser <- SessionUtils.userFromSessionToken

            appName <- HttpRequest.query.get[String]("app-name")
            app <- Misc.appByName(dbUser, appName)
            dbLogs <- LogStorage.forAppId(app.id)
          } yield HttpResponse.encodeJson(dbLogs.map(DbToDomain.log))
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
            dbLogs <- LogStorage.allForQuery(dbUser.id, convertedQuery.toLogFilterFunction)
          } yield HttpResponse.encodeJson(dbLogs.map(DbToDomain.log).sortBy(_.dateTime).reverse)
        }
      },
    )

}
