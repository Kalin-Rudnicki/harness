package template.domain.session

import harness.zio.*
import template.api.model as Api
import template.domain.model.{DomainError, Session, User}
import zio.*

trait SessionService {
  def isSecure: UIO[Boolean]
  def tokenKey: UIO[String]
  def getUserAllowUnverifiedEmail(token: Api.user.UserToken): ZIO[Logger & Telemetry, DomainError, User]
  def getUser(token: Api.user.UserToken): ZIO[Logger & Telemetry, DomainError, User]
  def getSession(token: Api.user.UserToken): ZIO[Logger & Telemetry, DomainError, Session]
}
