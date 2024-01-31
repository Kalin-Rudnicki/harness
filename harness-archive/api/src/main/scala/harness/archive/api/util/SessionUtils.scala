package harness.archive.api.util

import cats.syntax.option.*
import harness.archive.api.db.model as M
import harness.archive.api.service.storage.*
import harness.core.*
import harness.http.server.*
import harness.web.*
import harness.zio.*
import zio.*

object SessionUtils {

  val SessionToken: String = "Archive-Session-Token"

  val isSecure: Boolean = false

  private val getSessionToken: HRIO[HttpRequest, Option[String]] =
    HttpRequest.cookie
      .find[String](SessionUtils.SessionToken)
      .flatMap {
        case Some(value) => ZIO.some(value)
        case None        => HttpRequest.header.find[String](SessionUtils.SessionToken)
      }

  private def withSessionTokenOptional[T](
      f: String => HRIO[SessionStorage & Logger & Telemetry, Option[T]],
  ): HRIO[SessionStorage & Logger & Telemetry & HttpRequest, Option[T]] =
    getSessionToken.flatMap {
      case Some(tok) =>
        f(tok).flatMap {
          case Some(res) => ZIO.some(res)
          case None =>
            val msg = "Session token was specified, but is not valid"
            Logger.log.warning(msg) *>
              HttpResponse.earlyReturn(HttpResponse.encodeJson(List(msg), HttpCode.`401`).withCookie(Cookie.unset(SessionUtils.SessionToken).rootPath.secure(isSecure)))
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

  private val appTokenHeader = "ARCHIVE-APP-TOKEN"

  val getAppToken: HRIO[AppTokenStorage & Logger & Telemetry & HttpRequest, M.AppToken.Identity] =
    for {
      appToken <- HttpRequest.header.find[String](appTokenHeader).someOrFail {
        HError.UserError(s"Unauthorized: Specify header '$appTokenHeader'").withHTTPCode(HttpCode.`401`)
      }
      dbAppToken <- AppTokenStorage.fromToken(appToken).someOrFail {
        HError.UserError(s"Invalid '$appTokenHeader'").withHTTPCode(HttpCode.`401`)
      }
    } yield dbAppToken

}
