package harness.archive.api.util

import harness.archive.api.db.model as M
import harness.archive.api.service.storage.*
import harness.core.*
import harness.http.server.*
import harness.sql.*
import harness.web.*
import harness.zio.*
import zio.*

object SessionUtils {

  val SessionToken: String = "Archive-Session-Token"

  private def withSessionTokenOptional[T](
      f: String => HRIO[SessionStorage & Logger & Telemetry, Option[T]],
  ): HRIO[SessionStorage & Logger & Telemetry & HttpRequest, Option[T]] =
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

  val sessionFromSessionTokenOptional: HRIO[SessionStorage & Logger & Telemetry & HttpRequest, Option[M.Session.Identity]] =
    withSessionTokenOptional(SessionStorage.sessionFromSessionToken)

  val sessionFromSessionToken: HRIO[SessionStorage & Logger & Telemetry & HttpRequest, M.Session.Identity] =
    sessionFromSessionTokenOptional.someOrFail {
      HError.UserError(s"Unauthorized: Specify cookie '$SessionToken'").withHTTPCode(HttpCode.`401`)
    }

  val userFromSessionTokenOptional: HRIO[SessionStorage & Logger & Telemetry & HttpRequest, Option[M.User.Identity]] =
    withSessionTokenOptional(SessionStorage.userFromSessionToken)

  val userFromSessionToken: HRIO[SessionStorage & Logger & Telemetry & HttpRequest, M.User.Identity] =
    userFromSessionTokenOptional.someOrFail {
      HError.UserError(s"Unauthorized: Specify cookie '$SessionToken'").withHTTPCode(HttpCode.`401`)
    }

}
