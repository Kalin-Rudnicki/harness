package harness.sql

import harness.sql.query.*
import shapeless3.deriving.Id as Identity

abstract class TableQueries[Id, T[F[_]] <: Table.WithId[F, Id]](implicit ti: TableSchema[T]) {

  final val insert: QueryI[T[Identity]] =
    Prepare.insertO {
      Insert.into[T]
    }

  final val selectAll: QueryO[T[Identity]] =
    Prepare.selectO {
      Select.from[T](ti.tableName.head.toString).returning { t => t }
    }

  final val selectById: QueryIO[Id, T[Identity]] =
    Prepare.selectIO { Input[Id] } { pk =>
      Select
        .from[T](ti.tableName.head.toString)
        .where { t => t.id === pk }
        .returning { t => t }
    }

  // TODO (KR) : update

  final val deleteById: QueryIO[Id, T[Identity]] =
    Prepare.deleteIO { Input[Id] } { pk =>
      Delete
        .from[T](ti.tableName.head.toString)
        .where { t => t.id === pk }
        .returning { t => t }
    }

}
