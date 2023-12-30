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

  val routes: Route[AppStorage & LogStorage & Transaction] =
    "log" /: Route.oneOf(
      (HttpMethod.POST / "upload").implement { _ =>
        Transaction.inTransaction {
          for {
            // TODO (KR) :
            _ <- Misc.warnUserPermissions
            // dbUser <- SessionUtils.userFromSession

            body <- HttpRequest.jsonBody[Chunk[D.log.Upload]]
            _ <- Logger.log.info(s"Received request to create ${body.length.pluralizeOn("log")}")
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
      (HttpMethod.GET / "get-for-app").implement { _ =>
        Transaction.inTransaction {
          for {
            // TODO (KR) :
            _ <- Misc.warnUserPermissions
            // dbUser <- SessionUtils.userFromSession

            appName <- HttpRequest.query.get[String]("app-name")
            app <- Misc.appByName(appName)
            dbLogs <- LogStorage.forAppId(app.id)
          } yield HttpResponse.encodeJson(dbLogs.map(DbToDomain.log))
        }
      },
      (HttpMethod.GET / "get").implement { _ =>
        Transaction.inTransaction {
          for {
            // TODO (KR) :
            _ <- Misc.warnUserPermissions
            // dbUser <- SessionUtils.userFromSession

            _ <- HttpRequest.query.logAll(Logger.LogLevel.Detailed)

            query <- HttpRequest.query.get[String]("query")
            source = Source(query, None)
            _ <- ZIO.foreachDiscard(QueryParser.lexer.tokenize(source).toOption) { tokens =>
              Logger.log.info(Source.markAll(tokens.map(tok => Marked(tok.text, tok.span))))
            }
            parsedQuery <- QueryParser.parse(source) match {
              case Right(value) => ZIO.succeed(value)
              case Left(errors) => ZIO.fail(HError.UserError(Source.markAll(errors.toList)))
            }
            convertedQuery <- ZIO.eitherNelToUserErrors(ParsedQuery.from(parsedQuery.toExpr).leftMap(NonEmptyList.one))
            _ <- Logger.log.info(s"query:\n$query\n${parsedQuery.toExpr}\n$convertedQuery")
            dbLogs <- LogStorage.allForQuery(convertedQuery.toLogFilterFunction)
          } yield HttpResponse.encodeJson(dbLogs.map(DbToDomain.log).sortBy(_.dateTime).reverse)
        }
      },
    )

}
