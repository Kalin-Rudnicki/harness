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
        def gen: Id = UUID.randomUUID

        /**
          * It is recommended to use [[pkCol]] or [[fkCol]] instead,
          * because these are the only 2 situations a col of this type should be used.
          */
        def basicCol(colName: String): Col[Id] =
          Col.uuid(colName).imap(Id(_))(_.toUUID)

        inline def pkCol: Col[Id] =
          Id.pkCol("id")
        inline def pkCol(colName: String): Col[Id] =
          Id.basicCol(colName).primaryKey

        inline def fkCol(colName: String): Col[Id] =
          Id.fkCol(colName, "id")
        inline def fkCol(colName: String, referencesColName: String): Col[Id] =
          Id.basicCol(colName).references(ForeignKeyRef(tableSchema.tableSchema, tableSchema.tableName, referencesColName))
      }

    }

  }

}
