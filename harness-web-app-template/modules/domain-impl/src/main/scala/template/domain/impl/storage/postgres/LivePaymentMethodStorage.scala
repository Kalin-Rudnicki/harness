package template.domain.impl.storage.postgres

import harness.sql.*
import harness.sql.query.{*, given}
import harness.zio.*
import template.api.model as Api
import template.db.model as Db
import template.domain.impl.storage.postgres.StorageUtils.*
import template.domain.model.DomainError
import template.domain.model as Domain
import template.domain.storage.PaymentMethodStorage
import zio.*

final case class LivePaymentMethodStorage(db: Database) extends PaymentMethodStorage {
  import LivePaymentMethodStorage.Q

  override def insert(paymentMethod: Domain.PaymentMethod): IO[DomainError, Unit] =
    db.use { Q.insert(Db.PaymentMethod.fromDomain(paymentMethod)).single }.mapError(DomainError.UnexpectedStorageError(_))

  override def getById(id: Api.paymentMethod.PaymentMethodId): IO[DomainError, Domain.PaymentMethod] =
    db.use { Q.selectById(id).single[DomainError](DomainError.MissingExpectedInStorage(id.toString)) }.map(Db.PaymentMethod.toDomain)

  override def getForUser(userId: Api.user.UserId): IO[DomainError, Chunk[Domain.PaymentMethod]] =
    db.use { Q.forUser(userId).chunk }.mapBoth(DomainError.UnexpectedStorageError(_), _.map(Db.PaymentMethod.toDomain))

}
object LivePaymentMethodStorage {

  val liveLayer: URLayer[Database, PaymentMethodStorage] =
    ZLayer.fromFunction { LivePaymentMethodStorage.apply }

  private object Q extends TableQueries[Db.PaymentMethod.Id, Db.PaymentMethod] {

    val forUser: QueryIO[Api.user.UserId, Db.PaymentMethod.Identity] =
      Prepare.selectIO("PaymentMethodStorage - forUser") { Input[Db.User.Id] } { userId =>
        Select
          .from[Db.PaymentMethod]("pm")
          .where { pm => pm.userId === userId }
          .returning { pm => pm }
      }

  }

}
