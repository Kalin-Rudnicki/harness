package template.domain.impl.storage.inMemory

import harness.sql.mock.*
import template.api.model as Api
import template.domain.model.*
import template.domain.storage.PaymentMethodStorage
import zio.*

final case class InMemoryPaymentMethodStorage(state: MockState[DomainError, DbState]) extends PaymentMethodStorage {

  override def insert(paymentMethod: PaymentMethod): IO[DomainError, Unit] =
    state.focusAndUpdateW(_.paymentMethods) { (a, b) => a.users.PK.get(paymentMethod.userId) *> (b + paymentMethod) }

  override def getById(id: Api.paymentMethod.PaymentMethodId): IO[DomainError, PaymentMethod] =
    state.getWith(_.paymentMethods.PK.get(id))

  override def getForUser(userId: Api.user.UserId): IO[DomainError, Chunk[PaymentMethod]] =
    state.get.map { _.paymentMethods.ForUserIndex.find(userId) }

}
object InMemoryPaymentMethodStorage {

  val layer: URLayer[MockState[DomainError, DbState], InMemoryPaymentMethodStorage] =
    ZLayer.fromFunction { InMemoryPaymentMethodStorage.apply }

}
