package harness.archive.domain.impl.storage.inMemory

import harness.archive.api.model as Api
import harness.archive.domain.model.*
import harness.archive.domain.storage.AppStorage
import harness.zio.*
import zio.*

// TODO (KR) : Create Contract + Spec
final case class InMemoryAppStorage(stateRef: Ref.Synchronized[DbState]) extends AppStorage {

  override def insert(app: App): ZIO[Logger & Telemetry, DomainError, Unit] =
    stateRef.updateZIO { state =>
      for {
        _ <- state.apps.verifyKeyDne(app.id)
        _ <- state.users.get(app.userId)
        _ <- state.apps.UserAndNameIndex.verifyKeyDne((app.userId, app.name))
        updatedApps = state.apps.state.updated(app.id, app)
      } yield state.copy(apps = DbState.AppTable(updatedApps))
    }

  override def byId(id: Api.app.AppId): ZIO[Logger & Telemetry, DomainError, App] =
    stateRef.get.flatMap(_.apps.get(id))

  override def byName(userId: Api.user.UserId, name: String): ZIO[Logger & Telemetry, DomainError, Option[App]] =
    stateRef.get.map(_.apps.UserAndNameIndex.find((userId, name)))

  override def selectAll(userId: Api.user.UserId): ZIO[Logger & Telemetry, DomainError, Chunk[App]] =
    stateRef.get.map { state => Chunk.fromIterable(state.apps.UserIndex.find(userId)) }

}
object InMemoryAppStorage {

  val layer: URLayer[Ref.Synchronized[DbState], InMemoryAppStorage] =
    ZLayer.fromFunction { InMemoryAppStorage.apply }

}
