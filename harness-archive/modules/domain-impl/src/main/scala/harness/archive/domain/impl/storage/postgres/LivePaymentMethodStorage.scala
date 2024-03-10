package harness.archive.domain.impl.storage.postgres

import harness.sql.*
import harness.sql.query.{given, *}
import harness.zio.*
import harness.archive.api.model as Api
import harness.archive.db.model as Db
import harness.archive.domain.impl.storage.postgres.StorageUtils.*
import harness.archive.domain.model.DomainError
import harness.archive.domain.model as Domain
import harness.archive.domain.storage.PaymentMethodStorage
import zio.*

final case class LivePaymentMethodStorage(con: JDBCConnection) extends PaymentMethodStorage {
  import LivePaymentMethodStorage.Q

  override def insert(paymentMethod: Domain.PaymentMethod): ZIO[Logger & Telemetry, DomainError, Unit] =
    con.use { Q.insert(Db.PaymentMethod.fromDomain(paymentMethod)).single }.mapError(DomainError.UnexpectedStorageError(_))

  override def getById(id: Api.paymentMethod.PaymentMethodId): ZIO[Logger & Telemetry, DomainError, Domain.PaymentMethod] =
    con.use { Q.selectById(id).single[DomainError](DomainError.MissingExpectedInStorage(id.toString)) }.map(Db.PaymentMethod.toDomain)

  override def getForUser(userId: Api.user.UserId): ZIO[Logger & Telemetry, DomainError, Chunk[Domain.PaymentMethod]] =
    con.use { Q.forUser(userId).chunk }.mapBoth(DomainError.UnexpectedStorageError(_), _.map(Db.PaymentMethod.toDomain))

}
object LivePaymentMethodStorage {

  val liveLayer: URLayer[JDBCConnection, PaymentMethodStorage] =
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
