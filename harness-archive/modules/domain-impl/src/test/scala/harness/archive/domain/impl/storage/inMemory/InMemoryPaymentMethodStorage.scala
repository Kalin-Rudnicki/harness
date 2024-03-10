package harness.archive.domain.impl.storage.inMemory

import harness.zio.*
import harness.archive.api.model as Api
import harness.archive.domain.model.*
import harness.archive.domain.storage.PaymentMethodStorage
import zio.*

final case class InMemoryPaymentMethodStorage(stateRef: Ref.Synchronized[DbState]) extends PaymentMethodStorage {

  override def insert(paymentMethod: PaymentMethod): ZIO[Logger & Telemetry, DomainError, Unit] =
    stateRef.updateZIO { state =>
      for {
        _ <- state.users.get(paymentMethod.userId)
        _ <- state.paymentMethods.verifyKeyDne(paymentMethod.id)
        updatedPaymentMethods = state.paymentMethods.state.updated(paymentMethod.id, paymentMethod)
      } yield state.copy(paymentMethods = DbState.PaymentMethodTable(updatedPaymentMethods))
    }

  override def getById(id: Api.paymentMethod.PaymentMethodId): ZIO[Logger & Telemetry, DomainError, PaymentMethod] =
    stateRef.get.flatMap {
      _.paymentMethods.get(id)
    }

  override def getForUser(userId: Api.user.UserId): ZIO[Logger & Telemetry, DomainError, Chunk[PaymentMethod]] =
    stateRef.get.map { state =>
      Chunk.fromIterable(state.paymentMethods.ForUserIndex.find(userId))
    }

}
object InMemoryPaymentMethodStorage {

  val layer: URLayer[Ref.Synchronized[DbState], InMemoryPaymentMethodStorage] =
    ZLayer.fromFunction { InMemoryPaymentMethodStorage.apply }

}
