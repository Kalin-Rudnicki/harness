package template.domain.storage

import harness.payments.model.ids.*
import template.api.model as Api
import template.domain.model.*
import zio.*

trait UserStorage {
  def insert(user: User): IO[DomainError, Unit]
  def byUsername(username: String): IO[DomainError, Option[User]]
  def setEmailCodes(id: Api.user.UserId, codes: Option[Set[Api.user.EmailVerificationCode]]): IO[DomainError, Unit]
  def setStripeCustomerId(id: Api.user.UserId, customerId: Option[CustomerId]): IO[DomainError, Unit]
}
