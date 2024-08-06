package template.domain.storage

import template.api.model as Api
import template.domain.model.*
import zio.*

trait PaymentMethodStorage {
  def insert(paymentMethod: PaymentMethod): IO[DomainError, Unit]
  def getById(id: Api.paymentMethod.PaymentMethodId): IO[DomainError, PaymentMethod]
  def getForUser(userId: Api.user.UserId): IO[DomainError, Chunk[PaymentMethod]]
}
