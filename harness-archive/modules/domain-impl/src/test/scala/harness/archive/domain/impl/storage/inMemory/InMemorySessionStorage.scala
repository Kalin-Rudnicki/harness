package harness.archive.domain.impl.storage.inMemory

import harness.zio.*
import harness.archive.api.model as Api
import harness.archive.domain.model.*
import harness.archive.domain.storage.SessionStorage
import zio.*

final case class InMemorySessionStorage(stateRef: Ref.Synchronized[DbState]) extends SessionStorage {

  override def insert(session: Session): ZIO[Logger & Telemetry, DomainError, Unit] =
    stateRef.updateZIO { state =>
      for {
        _ <- state.users.get(session.userId)
        _ <- state.sessions.verifyKeyDne(session.id)
        _ <- state.sessions.TokenIndex.verifyKeyDne(session.token)
        updatedSessions = state.sessions.state.updated(session.id, session)
      } yield state.copy(sessions = DbState.SessionTable(updatedSessions))
    }

  override def sessionFromSessionToken(token: Api.user.UserToken): ZIO[Logger & Telemetry, DomainError, Option[Session]] =
    stateRef.get.map {
      _.sessions.TokenIndex.find(token)
    }

  override def userFromSessionToken(token: Api.user.UserToken): ZIO[Logger & Telemetry, DomainError, Option[User]] =
    (for {
      state <- stateRef.get
      session <- ZIO.succeed(state.sessions.TokenIndex.find(token)).some
      user <- ZIO.succeed(state.users.find(session.userId)).some
    } yield user).unsome

  override def deleteById(id: Api.user.SessionId): ZIO[Logger & Telemetry, DomainError, Unit] =
    stateRef.updateZIO { state =>
      for {
        _ <- state.sessions.get(id)
        updatedSessions = state.sessions.state.removed(id)
      } yield state.copy(sessions = DbState.SessionTable(updatedSessions))
    }

}
object InMemorySessionStorage {

  val layer: URLayer[Ref.Synchronized[DbState], InMemorySessionStorage] =
    ZLayer.fromFunction { InMemorySessionStorage.apply }

}
