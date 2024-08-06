package template.domain.storage

import template.api.model as Api
import template.domain.model.*
import zio.*

trait SessionStorage {
  def insert(session: Session): IO[DomainError, Unit]
  def sessionFromSessionToken(token: Api.user.UserToken): IO[DomainError, Option[Session]]
  def userFromSessionToken(token: Api.user.UserToken): IO[DomainError, Option[User]]
  def deleteById(id: Api.user.SessionId): IO[DomainError, Unit]
}
