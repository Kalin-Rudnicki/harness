package template.webServer.api

import harness.zio.*
import template.api.model as Api
import template.domain.model.*
import template.domain.storage.SessionStorage
import zio.*

object SessionUtils {

  def userFromSessionTokenAllowUnverifiedEmail(token: Api.user.UserToken, storage: SessionStorage): ZIO[Logger & Telemetry, DomainError, User] =
    storage
      .userFromSessionToken(token)
      .someOrFail(DomainError.InvalidSessionToken)

  def userFromSessionToken(token: Api.user.UserToken, storage: SessionStorage): ZIO[Logger & Telemetry, DomainError, User] =
    userFromSessionTokenAllowUnverifiedEmail(token, storage)
      .tap { user => ZIO.fail(DomainError.EmailNotVerified).when(user.verificationEmailCodes.nonEmpty) }

  def sessionFromSessionToken(token: Api.user.UserToken, storage: SessionStorage): ZIO[Logger & Telemetry, DomainError, Session] =
    storage
      .sessionFromSessionToken(token)
      .someOrFail(DomainError.InvalidSessionToken)

}
