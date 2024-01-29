package harness.archive.api.routes

import harness.archive.api.service.storage.*
import harness.archive.api.util.*
import harness.archive.model as D
import harness.http.server.{given, *}
import harness.sql.*
import harness.sql.query.Transaction
import harness.web.*
import zio.*

object App {

  val routes: Route[AppStorage & Transaction] =
    "app" /: Route.oneOf(
      (HttpMethod.GET / "get-all").implement { _ =>
        Transaction.inTransaction {
          for {
            // TODO (KR) :
            _ <- Misc.warnUserPermissions
            // dbUser <- SessionUtils.userFromSession

            apps <- AppStorage.selectAll
          } yield HttpResponse.encodeJson(apps.map(DbToDomain.app))
        }
      },
    )

}
