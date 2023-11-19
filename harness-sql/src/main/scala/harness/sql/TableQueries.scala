package harness.sql

import harness.pk.TableKey
import harness.sql.query.*
import shapeless3.deriving.Id as Identity

abstract class TableQueries[Id <: TableKey#Id, T[F[_]] <: Table.WithId[F, Id]](implicit ti: TableSchema[T]) {

  final val insert: QueryI[T[Identity]] =
    Prepare.insertO(s"${ti.referenceName} - insert") {
      Insert.into[T]
    }

  final val selectAll: QueryO[T[Identity]] =
    Prepare.selectO(s"${ti.referenceName} - selectAll") {
      Select.from[T](ti.referenceName.head.toString).returning { t => t }
    }

  final val selectById: QueryIO[Id, T[Identity]] =
    Prepare.selectIO(s"${ti.referenceName} - selectById") { Input[Id] } { pk =>
      Select
        .from[T](ti.referenceName.head.toString)
        .where { t => t.id === pk }
        .returning { t => t }
    }

  // TODO (KR) : update

  final val deleteById: QueryI[Id] =
    Prepare.deleteI(s"${ti.referenceName} - deleteById") { Input[Id] } { pk =>
      Delete
        .from[T](ti.referenceName.head.toString)
        .where { t => t.id === pk }
    }

  final val deleteByIdReturning: QueryIO[Id, T[Identity]] =
    Prepare.deleteIO(s"${ti.referenceName} - deleteByIdReturning") { Input[Id] } { pk =>
      Delete
        .from[T](ti.referenceName.head.toString)
        .where { t => t.id === pk }
        .returning { t => t }
    }

}
