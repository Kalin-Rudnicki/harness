package harness.sql

import harness.deriving.*
import harness.pk.TableKey
import harness.sql.query.*
import harness.sql.typeclass.*

abstract class TableQueries[Id <: TableKey#Id, T[F[_]] <: Table.WithId[F, Id]](implicit ti: TableSchema[T]) {

  final val insert: QueryI[T[K11.Identity]] =
    Prepare.insertO(s"${ti.referenceName} - insert") {
      Insert.into[T]
    }

  final val selectAll: QueryO[T[K11.Identity]] =
    Prepare.selectO(s"${ti.referenceName} - selectAll") {
      Select.from[T](ti.referenceName.head.toString).returning { t => t }
    }

  final val selectById: QueryIO[Id, T[K11.Identity]] =
    Prepare.selectIO(s"${ti.referenceName} - selectById") { Input[Id] } { pk =>
      Select
        .from[T](ti.referenceName.head.toString)
        .where { t => t.id === pk }
        .returning { t => t }
    }

  @scala.annotation.nowarn
  protected inline def makeUpdate(queryName: String)(set: T[K11.Const[Boolean]])(implicit gen: K11.ProductGeneric[T]): QueryI[T[K11.Identity]] = {
    val name = "t"
    val setCols = ti.flatten(set).zip(ti.colChunk).collect { case (true, col) => col }
    val setStr = setCols.map { col => s"${col.colName} = ?" }.mkString(", ")

    val setQim =
      QueryInputMapper.single(
        _(1).asInstanceOf[T[K11.Identity]],
        QueryEncoderMany.forTableFiltered(ti.functorK.mapK(ti.columns) { [a] => (c: Col[a]) => c.codec.encoder }, set),
      )
    val idQim =
      QueryInputMapper.single(
        _(0),
        QueryEncoderMany.fromSingle(ti.columns.id.codec.encoder.asInstanceOf[QueryEncoderSingle[Any]]),
      )

    new QueryI[T[K11.Identity]](
      queryName,
      Fragment(
        s"UPDATE ${ti.referenceName} $name SET $setStr WHERE ${AppliedCol(name, ti.columns.id).ref.show} = ?",
        setQim + idQim,
      ),
      (
        Input[Any] ~ Input[T[K11.Identity]]
      ).cmap[T[K11.Identity]] { t => (t.id, t) },
    )
  }

  final val deleteById: QueryI[Id] =
    Prepare.deleteI(s"${ti.referenceName} - deleteById") { Input[Id] } { pk =>
      Delete
        .from[T](ti.referenceName.head.toString)
        .where { t => t.id === pk }
    }

  final val deleteByIdReturning: QueryIO[Id, T[K11.Identity]] =
    Prepare.deleteIO(s"${ti.referenceName} - deleteByIdReturning") { Input[Id] } { pk =>
      Delete
        .from[T](ti.referenceName.head.toString)
        .where { t => t.id === pk }
        .returning { t => t }
    }

}
