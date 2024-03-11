package harness.archive.webServer.api

import harness.archive.api.model as Api
import harness.archive.domain.model.*
import harness.archive.domain.storage.SessionStorage
import harness.zio.*
import zio.*

object SessionUtils {

  def userFromSessionToken(token: Api.user.UserToken, storage: SessionStorage): ZIO[Logger & Telemetry, DomainError, User] =
    storage
      .userFromSessionToken(token)
      .someOrFail(DomainError.InvalidSessionToken)

  def sessionFromSessionToken(token: Api.user.UserToken, storage: SessionStorage): ZIO[Logger & Telemetry, DomainError, Session] =
    storage
      .sessionFromSessionToken(token)
      .someOrFail(DomainError.InvalidSessionToken)

}
