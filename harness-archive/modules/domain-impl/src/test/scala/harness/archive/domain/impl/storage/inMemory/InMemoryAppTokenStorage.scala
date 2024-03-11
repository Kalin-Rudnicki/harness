package harness.archive.domain.impl.storage.inMemory

import harness.archive.api.model as Api
import harness.archive.domain.model.*
import harness.archive.domain.storage.AppTokenStorage
import harness.zio.*
import zio.*

// TODO (KR) : Create Contract + Spec
final case class InMemoryAppTokenStorage(stateRef: Ref.Synchronized[DbState]) extends AppTokenStorage {

  override def insert(session: AppToken): ZIO[Logger & Telemetry, DomainError, Unit] =
    stateRef.updateZIO { state =>
      for {
        _ <- state.appTokens.verifyKeyDne(session.id)
        _ <- state.apps.get(session.appId)
        _ <- state.appTokens.TokenIndex.verifyKeyDne(session.token)
        updatedAppTokens = state.appTokens.state.updated(session.id, session)
      } yield state.copy(appTokens = DbState.AppTokenTable(updatedAppTokens))
    }

  override def fromToken(token: Api.app.AppToken): ZIO[Logger & Telemetry, DomainError, Option[AppToken]] =
    stateRef.get.map(_.appTokens.TokenIndex.find(token))

  override def deleteById(id: Api.app.AppTokenId): ZIO[Logger & Telemetry, DomainError, Unit] =
    stateRef.updateZIO { state =>
      for {
        _ <- state.appTokens.get(id)
        updatedAppTokens = state.appTokens.state.removed(id)
      } yield state.copy(appTokens = DbState.AppTokenTable(updatedAppTokens))
    }

}
object InMemoryAppTokenStorage {

  val layer: URLayer[Ref.Synchronized[DbState], InMemoryAppTokenStorage] =
    ZLayer.fromFunction { InMemoryAppTokenStorage.apply }

}
