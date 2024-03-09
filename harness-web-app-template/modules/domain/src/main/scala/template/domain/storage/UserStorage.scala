package template.domain.storage

import harness.payments.model.ids.*
import harness.zio.*
import template.api.model as Api
import template.domain.model.*
import zio.*

trait UserStorage {
  def insert(user: User): ZIO[Logger & Telemetry, DomainError, Unit]
  def byUsername(username: String): ZIO[Logger & Telemetry, DomainError, Option[User]]
  def setEmailCodes(id: Api.user.UserId, codes: Option[Set[Api.user.EmailVerificationCode]]): ZIO[Logger & Telemetry, DomainError, Unit]
  def setStripeCustomerId(id: Api.user.UserId, customerId: Option[CustomerId]): ZIO[Logger & Telemetry, DomainError, Unit]
}
