package template.domain.impl.session

import harness.zio.*
import template.api.model as Api
import template.domain.model.{DomainError, Session, User}
import template.domain.session.SessionService
import template.domain.storage.SessionStorage
import zio.*

final case class LiveSessionService(
    storage: SessionStorage,
    config: LiveSessionService.Config,
) extends SessionService {

  override def isSecure: UIO[Boolean] = ZIO.succeed(config.isSecure)

  override def tokenKey: UIO[String] = ZIO.succeed(config.tokenKey)

  override def getUserAllowUnverifiedEmail(token: Api.user.UserToken): ZIO[Logger & Telemetry, DomainError, User] =
    storage
      .userFromSessionToken(token)
      .someOrFail(DomainError.InvalidSessionToken)

  override def getUser(token: Api.user.UserToken): ZIO[Logger & Telemetry, DomainError, User] =
    getUserAllowUnverifiedEmail(token)
      .tap { user => ZIO.fail(DomainError.EmailNotVerified).when(user.verificationEmailCodes.nonEmpty) }

  override def getSession(token: Api.user.UserToken): ZIO[Logger & Telemetry, DomainError, Session] =
    storage
      .sessionFromSessionToken(token)
      .someOrFail(DomainError.InvalidSessionToken)

}
object LiveSessionService {

  final case class Config(isSecure: Boolean, tokenKey: String)

  val layer: URLayer[SessionStorage & Config, SessionService] =
    ZLayer.fromFunction { LiveSessionService.apply }

}
