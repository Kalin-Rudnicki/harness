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

  val isSecure: Boolean = false

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
              HttpResponse.earlyReturn(HttpResponse.encodeJson(List(msg), HttpCode.`401`).withCookie(Cookie.unset(SessionUtils.SessionToken).rootPath.secure(isSecure)))
        }
      case None => ZIO.none
    }

  extension [R, A](self: HRIO[R, Option[A]])
    def someOr401: HRIO[R, A] =
      self.someOrFail { HError.UserError(s"Unauthorized: Specify cookie '$SessionToken'").withHTTPCode(HttpCode.`401`) }

  val sessionFromSessionTokenOptional: HRIO[SessionStorage & Logger & Telemetry & HttpRequest, Option[M.Session.Identity]] =
    withSessionTokenOptional(SessionStorage.sessionFromSessionToken)

  val sessionFromSessionToken: HRIO[SessionStorage & Logger & Telemetry & HttpRequest, M.Session.Identity] =
    sessionFromSessionTokenOptional.someOr401

  val userFromSessionTokenOptional: HRIO[SessionStorage & Logger & Telemetry & HttpRequest, Option[M.User.Identity]] =
    withSessionTokenOptional(SessionStorage.userFromSessionToken)

  def userFromSessionToken(allowUnverifiedEmail: Boolean): HRIO[SessionStorage & Logger & Telemetry & HttpRequest, M.User.Identity] =
    userFromSessionTokenOptional.someOr401.flatMap { user =>
      if (user.verificationEmailCodes.isDefined && !allowUnverifiedEmail) ZIO.fail(HError.UserError("This action is not allowed until you verify your email address").withHTTPCode(HttpCode.`403`))
      else ZIO.succeed(user)
    }
  def userFromSessionToken: HRIO[SessionStorage & Logger & Telemetry & HttpRequest, M.User.Identity] =
    SessionUtils.userFromSessionToken(false)

}
