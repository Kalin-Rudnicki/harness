package template.domain.impl.storage.postgres

import harness.payments.model.ids.*
import harness.sql.*
import harness.sql.query.{*, given}
import template.api.model as Api
import template.db.model as Db
import template.domain.model.DomainError
import template.domain.model as Domain
import template.domain.storage.UserStorage
import zio.*

final case class LiveUserStorage(db: Database) extends UserStorage {
  import LiveUserStorage.Q

  override def insert(user: Domain.User): IO[DomainError, Unit] =
    db.use { Q.insert(Db.User.fromDomain(user)).single }.mapError(DomainError.UnexpectedStorageError(_))

  override def byUsername(username: String): IO[DomainError, Option[Domain.User]] =
    db.use { Q.byUsername(username).option }.mapBoth(DomainError.UnexpectedStorageError(_), _.map(Db.User.toDomain))

  override def setEmailCodes(id: Api.user.UserId, codes: Option[Set[Api.user.EmailVerificationCode]]): IO[DomainError, Unit] =
    db.use { Q.setEmailCodes(id, codes).single }.mapError(DomainError.UnexpectedStorageError(_))

  override def setStripeCustomerId(id: Api.user.UserId, customerId: Option[CustomerId]): IO[DomainError, Unit] =
    db.use { Q.setStripeCustomerId(id, customerId).single }.mapError(DomainError.UnexpectedStorageError(_))

}
object LiveUserStorage {

  val liveLayer: URLayer[Database, UserStorage] =
    ZLayer.fromFunction { LiveUserStorage.apply }

  private object Q extends TableQueries[Db.User.Id, Db.User] {

    val byUsername: QueryIO[String, Db.User.Identity] =
      Prepare
        .selectIO("User - byUsername") { Input[String] } { username =>
          Select
            .from[Db.User]("u")
            .where { u => u.lowerUsername === username }
            .returning { u => u }
        }
        .cmap[String](_.toLowerCase)

    val setEmailCodes: QueryI[(Api.user.UserId, Option[Set[Api.user.EmailVerificationCode]])] =
      Prepare.updateI("User - setEmailVerified") { Input[Db.User.Id] ~ Input[Option[Set[Api.user.EmailVerificationCode]]] } { case (userId, codes) =>
        Update[Db.User]("u")
          .where(_.id === userId)
          .set(_.verificationEmailCodes := codes)
      }

    val setStripeCustomerId: QueryI[(Api.user.UserId, Option[CustomerId])] =
      Prepare.updateI("User - setStripeCustomerId") { Input[Db.User.Id] ~ Input[Option[CustomerId]] } { case (userId, customerId) =>
        Update[Db.User]("u")
          .where(_.id === userId)
          .set(_.stripeCustomerId := customerId)
      }

  }

}
