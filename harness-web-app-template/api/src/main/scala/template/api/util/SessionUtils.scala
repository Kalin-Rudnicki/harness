package template.api.util

import harness.core.*
import harness.http.server.*
import harness.sql.*
import harness.web.*
import harness.zio.*
import template.api.db.model as M
import template.api.service.storage.*
import template.model as D
import zio.*

object SessionUtils {

  // TODO (KR) : name this on a project specific basis
  val SessionToken: String = "Template-Session-Token"

  private def withSessionTokenOptional[T](
      f: String => HRIO[SessionStorage & JDBCConnection & Logger & Telemetry, Option[T]],
  ): HRIO[SessionStorage & JDBCConnection & Logger & Telemetry & HttpRequest, Option[T]] =
    HttpRequest.cookie.find[String](SessionUtils.SessionToken).flatMap {
      case Some(tok) =>
        f(tok).flatMap {
          case Some(res) => ZIO.some(res)
          case None =>
            val msg = "Session token was specified, but is not valid"
            Logger.log.warning(msg) *>
              HttpResponse.earlyReturn(HttpResponse.encodeJson(List(msg), HttpCode.`401`))
        }
      case None => ZIO.none
    }

  val sessionFromSessionTokenOptional: HRIO[SessionStorage & JDBCConnection & Logger & Telemetry & HttpRequest, Option[M.Session.Identity]] =
    withSessionTokenOptional(SessionStorage.sessionFromSessionToken)

  val sessionFromSessionToken: HRIO[SessionStorage & JDBCConnection & Logger & Telemetry & HttpRequest, M.Session.Identity] =
    sessionFromSessionTokenOptional.someOrFail {
      HError.UserError(s"Unauthorized: Specify cookie '$SessionToken'").withHTTPCode(HttpCode.`401`)
    }

  val userFromSessionTokenOptional: HRIO[SessionStorage & JDBCConnection & Logger & Telemetry & HttpRequest, Option[M.User.Identity]] =
    withSessionTokenOptional(SessionStorage.userFromSessionToken)

  val userFromSessionToken: HRIO[SessionStorage & JDBCConnection & Logger & Telemetry & HttpRequest, M.User.Identity] =
    userFromSessionTokenOptional.someOrFail {
      HError.UserError(s"Unauthorized: Specify cookie '$SessionToken'").withHTTPCode(HttpCode.`401`)
    }

}
