package template.domain.impl.storage.inMemory

import harness.payments.model.ids.*
import harness.sql.mock.*
import harness.zio.*
import template.api.model as Api
import template.domain.model.*
import template.domain.storage.UserStorage
import zio.*

final case class InMemoryUserStorage(state: MockState[DomainError, DbState]) extends UserStorage {

  override def insert(user: User): ZIO[Logger & Telemetry, DomainError, Unit] =
    state.focusAndUpdate(_.users) { _ + user }

  override def byUsername(username: String): ZIO[Logger & Telemetry, DomainError, Option[User]] =
    state.get.map { _.users.UsernameIndex.find(username.toLowerCase) }

  override def setEmailCodes(id: Api.user.UserId, codes: Option[Set[Api.user.EmailVerificationCode]]): ZIO[Logger & Telemetry, DomainError, Unit] =
    state.focusAndUpdate(_.users) { _.PK.updated(id) { _.copy(verificationEmailCodes = codes) } }

  override def setStripeCustomerId(id: Api.user.UserId, customerId: Option[CustomerId]): ZIO[Logger & Telemetry, DomainError, Unit] =
    state.focusAndUpdate(_.users) { _.PK.updated(id) { _.copy(stripeCustomerId = customerId) } }

}
object InMemoryUserStorage {

  val layer: URLayer[MockState[DomainError, DbState], InMemoryUserStorage] =
    ZLayer.fromFunction { InMemoryUserStorage.apply }

}
