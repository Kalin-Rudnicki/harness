package template.domain.impl.storage.inMemory

import harness.sql.mock.*
import harness.zio.*
import template.api.model as Api
import template.domain.model.*
import template.domain.storage.SessionStorage
import zio.*

final case class InMemorySessionStorage(state: MockState[DomainError, DbState]) extends SessionStorage {

  override def insert(session: Session): ZIO[Logger & Telemetry, DomainError, Unit] =
    state.focusAndUpdateW(_.sessions) { (a, b) => a.users.PK.get(session.userId) *> (b + session) }

  override def sessionFromSessionToken(token: Api.user.UserToken): ZIO[Logger & Telemetry, DomainError, Option[Session]] =
    state.get.map { _.sessions.TokenIndex.find(token) }

  override def userFromSessionToken(token: Api.user.UserToken): ZIO[Logger & Telemetry, DomainError, Option[User]] =
    state.get.map { state =>
      state.sessions.TokenIndex.find(token).flatMap { session =>
        state.users.PK.find(session.userId)
      }
    }

  override def deleteById(id: Api.user.SessionId): ZIO[Logger & Telemetry, DomainError, Unit] =
    state.focusAndUpdate(_.sessions) { _.PK.removed(id) }

}
object InMemorySessionStorage {

  val layer: URLayer[MockState[DomainError, DbState], InMemorySessionStorage] =
    ZLayer.fromFunction { InMemorySessionStorage.apply }

}
