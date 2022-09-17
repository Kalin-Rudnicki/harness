package harness.sql

import harness.sql.query.*
import java.util.UUID

abstract class Table
object Table {

  abstract class WithId[F[_], Id] extends Table {
    val id: F[Id]
  }

  trait Companion[T[_[_]] <: Table] {

    final type Identity = T[shapeless3.deriving.Id]
    final type Cols = T[Col]

    implicit lazy val tableSchema: TableSchema[T]

  }
  object Companion {

    trait WithId[T[_[_]] <: Table] extends Table.Companion[T] {

      opaque type Id = UUID
      extension (self: Id) {
        def toUUID: UUID = self
      }
      object Id {
        def apply(uuid: UUID): Id = uuid
        def col(colName: String): Col[Id] = Col.uuid(colName).imap(Id(_))(_.toUUID)
        def gen: Id = UUID.randomUUID
      }

    }

  }

}
