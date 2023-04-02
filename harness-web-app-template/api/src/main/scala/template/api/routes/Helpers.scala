package template.api.routes

import harness.core.*
import harness.sql.*
import harness.web.*
import harness.web.server.*
import harness.zio.*
import template.api.db.{model as M, queries as Q}
import zio.*

private[routes] object Helpers {

  // TODO (KR) : name this on a project specific basis
  val SessionToken: String = "Template-Session-Token"

  val userFromSessionOptional: HRIO[JDBCConnection & Logger & Telemetry & HttpRequest, Option[M.User.Identity]] =
    HttpRequest.cookie.find[String](Helpers.SessionToken).flatMap {
      case Some(tok) =>
        Q.User.fromSessionToken(tok).option.flatMap {
          case Some(tok) => ZIO.some(tok)
          case None =>
            Logger.log.warning("Session token was specified, but is not valid") *>
              HttpResponse.earlyReturn.fromHttpCode.json(HttpCode.`401`)
        }
      case None => ZIO.none
    }

  val userFromSession: HRIO[JDBCConnection & Logger & Telemetry & HttpRequest, M.User.Identity] =
    userFromSessionOptional.someOrElseZIO {
      ZIO.fail(HError.UserError(s"Unauthorized: Specify cookie '$SessionToken'").withHTTPCode(HttpCode.`401`))
    }

}
