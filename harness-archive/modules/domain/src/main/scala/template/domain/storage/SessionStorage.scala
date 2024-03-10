package template.domain.storage

import harness.zio.*
import template.api.model as Api
import template.domain.model.*
import zio.*

trait SessionStorage {
  def insert(session: Session): ZIO[Logger & Telemetry, DomainError, Unit]
  def sessionFromSessionToken(token: Api.user.UserToken): ZIO[Logger & Telemetry, DomainError, Option[Session]]
  def userFromSessionToken(token: Api.user.UserToken): ZIO[Logger & Telemetry, DomainError, Option[User]]
  def deleteById(id: Api.user.SessionId): ZIO[Logger & Telemetry, DomainError, Unit]
}
