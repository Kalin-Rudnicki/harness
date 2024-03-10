package template.domain.impl.storage.inMemory

import harness.payments.model.ids.*
import harness.zio.*
import template.api.model as Api
import template.domain.model.*
import template.domain.storage.UserStorage
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

  override def setEmailCodes(id: Api.user.UserId, codes: Option[Set[Api.user.EmailVerificationCode]]): ZIO[Logger & Telemetry, DomainError, Unit] =
    stateRef.updateZIO { state =>
      for {
        user <- ZIO.getOrFailWith(DomainError.UnexpectedStorageError.fromMessage("No such user to update"))(state.users.state.get(id))
        updatedUser = user.copy(verificationEmailCodes = codes)
        updatedUsers = state.users.state.updated(updatedUser.id, updatedUser)
      } yield state.copy(users = DbState.UserTable(updatedUsers))
    }

  override def setStripeCustomerId(id: Api.user.UserId, customerId: Option[CustomerId]): ZIO[Logger & Telemetry, DomainError, Unit] =
    stateRef.updateZIO { state =>
      for {
        user <- state.users.get(id)
        updatedUser = user.copy(stripeCustomerId = customerId)
        updatedUsers = state.users.state.updated(updatedUser.id, updatedUser)
      } yield state.copy(users = DbState.UserTable(updatedUsers))
    }

}
object InMemoryUserStorage {

  val layer: URLayer[Ref.Synchronized[DbState], InMemoryUserStorage] =
    ZLayer.fromFunction { InMemoryUserStorage.apply }

}
