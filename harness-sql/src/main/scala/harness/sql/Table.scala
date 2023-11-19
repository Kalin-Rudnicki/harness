package harness.sql

import harness.core.*
import harness.pk.TableKey
import harness.sql.query.*
import java.util.UUID
import zio.*

abstract class Table
object Table {

  abstract class WithId[F[_], TKId <: TableKey#Id] extends Table {
    final type Id = TKId
    val id: F[Id]
  }

  trait Companion[T[_[_]] <: Table] {

    final type Identity = T[shapeless3.deriving.Id]
    final type Cols = T[Col]

    implicit lazy val tableSchema: TableSchema[T]

  }
  object Companion {

    trait WithId[TKId <: TableKey#Id, T[_[_]] <: Table.WithId[_, TKId]](implicit
        iMap: IMap[UUID, TKId],
    ) extends Table.Companion[T] {

      final type Id = TKId
      object Id {
        def apply(uuid: UUID): Id = iMap.to(uuid)
        def gen: Id = Id(UUID.randomUUID)
        def genZio: UIO[Id] = Random.nextUUID.map(Id(_))

        /**
          * It is recommended to use [[pkCol]] or [[fkCol]] instead,
          * because these are the only 2 situations a col of this type should be used.
          */
        def basicCol(colName: String): Col[Id] =
          Col.uuid(colName).imap(Id(_))(_.toUUID)

        def pkCol: Col[Id] =
          Id.pkCol("id")
        def pkCol(colName: String): Col[Id] =
          Id.basicCol(colName).primaryKey

        def fkCol: Col[Id] =
          Id.fkCol(s"${tableSchema.tableName}_id")
        def fkCol(colName: String): Col[Id] =
          Id.fkCol(colName, "id")
        def fkCol(colName: String, referencesColName: String): Col[Id] =
          Id.basicCol(colName).references(ForeignKeyRef(tableSchema.tableSchema, tableSchema.tableName, referencesColName))
      }

    }

  }

}
