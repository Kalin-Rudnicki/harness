package template.domain.impl.storage.inMemory

import template.api.model as Api
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
      users = UserTable(Map.empty),
      sessions = SessionTable(Map.empty),
      paymentMethods = PaymentMethodTable(Map.empty),
    )

  val layer: ULayer[Ref.Synchronized[DbState]] =
    ZLayer.fromZIO(Ref.Synchronized.make(DbState.empty))

  abstract class Table[K, V] { self =>

    val state: Map[K, V]

    final def find(key: K): Option[V] = state.get(key)
    final def get(key: K): IO[DomainError, V] =
      ZIO.getOrFailWith(DomainError.MissingExpectedInStorage(key.toString))(state.get(key))

    final def verifyKeyDne(key: K): IO[DomainError.UnexpectedStorageError, Unit] =
      if (state.contains(key)) ZIO.fail(DomainError.UnexpectedStorageError.fromMessage(s"${self.getClass.getSimpleName} key already exists: $key"))
      else ZIO.unit

    abstract class UniqueIndex[I](f: V => I) { self2 =>
      private val index: Map[I, V] = state.values.map(v => f(v) -> v).toMap
      final def find(i: I): Option[V] = index.get(i)
      final def get(key: I): IO[DomainError, V] =
        ZIO.getOrFailWith(DomainError.MissingExpectedInStorage(key.toString))(find(key))

      final def verifyKeyDne(key: I): IO[DomainError.UnexpectedStorageError, Unit] =
        if (index.contains(key)) ZIO.fail(DomainError.UnexpectedStorageError.fromMessage(s"${self.getClass.getSimpleName}.${self2.getClass.getSimpleName} key already exists: $key"))
        else ZIO.unit
    }

    abstract class ManyIndex[I](f: V => I) {
      private val index: Map[I, List[V]] = state.values.toList.groupBy(f)
      final def find(i: I): List[V] = index.getOrElse(i, Nil)
    }

  }

  final case class UserTable(state: Map[Api.user.UserId, User]) extends Table[Api.user.UserId, User] {
    object UsernameIndex extends UniqueIndex[String](_.lowerUsername)
  }

  final case class SessionTable(state: Map[Api.user.SessionId, Session]) extends Table[Api.user.SessionId, Session] {
    object TokenIndex extends UniqueIndex[Api.user.UserToken](_.token)
  }

  final case class PaymentMethodTable(state: Map[Api.paymentMethod.PaymentMethodId, PaymentMethod]) extends Table[Api.paymentMethod.PaymentMethodId, PaymentMethod] {
    object ForUserIndex extends ManyIndex[Api.user.UserId](_.userId)
  }

}
