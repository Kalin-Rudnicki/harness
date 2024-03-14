package template.domain.impl.storage.inMemory

import harness.sql.mock.*
import harness.zio.*
import template.domain.model.*
import zio.*

final case class DbState(
    users: DbState.UserTable,
    sessions: DbState.SessionTable,
    paymentMethods: DbState.PaymentMethodTable,
)
object DbState {

  val empty: DbState =
    DbState(
      MockTable.empty,
      MockTable.empty,
      MockTable.empty,
    )

  private implicit val em: ErrorMapper[Throwable, DomainError] = DomainError.UnexpectedStorageError(_)
  val layer: ULayer[MockState[DomainError, DbState]] =
    MockState.layer { DbState.empty }

  final class UserTable private (values: Chunk[User]) extends MockTable[User, UserTable]("User", values) {
    val PK = primaryKeyIndex(_.id)
    val UsernameIndex = UniqueIndex[String]("UsernameIndex", _.lowerUsername)
  }
  object UserTable {
    implicit val builder: MockTable.Builder[User, UserTable] = MockTable.Builder(new UserTable(_))
  }

  final class SessionTable private (values: Chunk[Session]) extends MockTable[Session, SessionTable]("Session", values) {
    val PK = primaryKeyIndex(_.id)
    val TokenIndex = UniqueIndex("TokenIndex", _.token)
  }
  object SessionTable {
    implicit val builder: MockTable.Builder[Session, SessionTable] = MockTable.Builder(new SessionTable(_))
  }

  final class PaymentMethodTable private (values: Chunk[PaymentMethod]) extends MockTable[PaymentMethod, PaymentMethodTable]("PaymentMethod", values) {
    val PK = primaryKeyIndex(_.id)
    val ForUserIndex = ManyIndex("ForUserIndex", _.userId)
  }
  object PaymentMethodTable {
    implicit val builder: MockTable.Builder[PaymentMethod, PaymentMethodTable] = MockTable.Builder(new PaymentMethodTable(_))
  }

}
