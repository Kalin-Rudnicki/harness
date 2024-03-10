package template.domain.storage

import harness.zio.*
import template.api.model as Api
import template.domain.model.*
import zio.*

trait PaymentMethodStorage {
  def insert(paymentMethod: PaymentMethod): ZIO[Logger & Telemetry, DomainError, Unit]
  def getById(id: Api.paymentMethod.PaymentMethodId): ZIO[Logger & Telemetry, DomainError, PaymentMethod]
  def getForUser(userId: Api.user.UserId): ZIO[Logger & Telemetry, DomainError, Chunk[PaymentMethod]]
}
