package harness.sql

import harness.sql.query.*
import java.util.UUID

abstract class Table
object Table {

  abstract class WithId[F[_]] extends Table {
    val id: F[UUID]
  }

  trait Companion[T[_[_]] <: Table] {

    final type Id = T[shapeless3.deriving.Id]

    implicit val tableSchema: TableSchema[T]

    final lazy val insert: QueryI[Id] =
      Prepare.insertO {
        Insert.into[T]
      }

    final lazy val selectAll: QueryO[Id] =
      Prepare.selectO {
        Select.from[T](tableSchema.tableName.head.toString).returning { t => t }
      }

  }
  object Companion {

    trait WithId[T[F[_]] <: Table.WithId[F]] extends Table.Companion[T] {

      // TODO (KR) : update

      final lazy val delete: QueryIO[UUID, Id] =
        Prepare.deleteIO(Input[UUID]) { id =>
          Delete
            .from[T](tableSchema.tableName.head.toString)
            .where { t => t.id === id }
            .returning { t => t }
        }

    }

  }

}
