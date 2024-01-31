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
import java.util.UUID
import zio.*

object App {

  val routes: Route[AppStorage & SessionStorage & AppTokenStorage & Transaction] =
    "app" /: Route.oneOf(
      (HttpMethod.GET / "get-all").implement { _ =>
        Transaction.inTransaction {
          for {
            dbUser <- SessionUtils.userFromSessionToken
            apps <- AppStorage.selectAll(dbUser.id)
          } yield HttpResponse.encodeJson(apps.map(DbToDomain.app))
        }
      },
      (HttpMethod.POST / "create").implement { _ =>
        Transaction.inTransaction {
          for {
            dbUser <- SessionUtils.userFromSessionToken
            appName <- HttpRequest.query.get[String]("app-name")
            existingApp <- AppStorage.byName(dbUser.id, appName)
            _ <- ZIO.fail(HError.UserError(s"App with name '$appName' already exists")).when(existingApp.nonEmpty)

            dbApp = Misc.makeApp(dbUser, appName)
            _ <- AppStorage.insert(dbApp)
          } yield HttpResponse.encodeJson(DbToDomain.app(dbApp))
        }
      },
      (HttpMethod.POST / "generate-api-token").implement { _ =>
        Transaction.inTransaction {
          for {
            now <- Clock.currentDateTime
            dbUser <- SessionUtils.userFromSessionToken
            appId <- HttpRequest.query.get[D.app.AppId]("app-id")
            tokenName <- HttpRequest.query.get[String]("token-name")

            app <- AppStorage.byId(appId)
            _ <- HttpResponse.earlyReturn.fromHttpCode.json(HttpCode.Forbidden).unless(app.userId == dbUser.id)

            dbAppToken = new M.AppToken.Identity(M.AppToken.Id.gen, app.id, tokenName, now, s"${UUID.randomUUID}:${UUID.randomUUID}")
            _ <- AppTokenStorage.insert(dbAppToken)
          } yield HttpResponse.encodeJson((DbToDomain.appToken(dbAppToken), dbAppToken.token))
        }
      },
    )

}
