package harness.archive.domain.impl.storage.inMemory

import harness.archive.domain.model.*
import harness.archive.domain.storage.UserStorage
import harness.zio.*
import zio.*

final case class InMemoryUserStorage(stateRef: Ref.Synchronized[DbState]) extends UserStorage {

  override def insert(user: User): ZIO[Logger & Telemetry, DomainError, Unit] =
    stateRef.updateZIO { state =>
      for {
        _ <- state.users.verifyKeyDne(user.id)
        _ <- state.users.UsernameIndex.verifyKeyDne(user.lowerUsername)
        updatedUsers = state.users.state.updated(user.id, user)
      } yield state.copy(users = DbState.UserTable(updatedUsers))
    }

  override def byUsername(username: String): ZIO[Logger & Telemetry, DomainError, Option[User]] =
    stateRef.get.map {
      _.users.UsernameIndex.find(username.toLowerCase)
    }

}
object InMemoryUserStorage {

  val layer: URLayer[Ref.Synchronized[DbState], InMemoryUserStorage] =
    ZLayer.fromFunction { InMemoryUserStorage.apply }

}
