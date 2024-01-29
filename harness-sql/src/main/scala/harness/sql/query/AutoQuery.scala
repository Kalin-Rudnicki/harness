package harness.sql.query

import harness.sql.*
import harness.sql.typeclass.{ColEncoder, RowEncoder}
import shapeless3.deriving.*

object AutoQuery {

  inline def update[T[F[_]] <: Table.WithId[F, ?]](set: T[Const[Boolean]])(using inst: K11.ProductGeneric[T], ti: TableSchema[T]): QueryI[T[Id]] = {
    val name = "t"
    val setCols = ti.tableToList.toList(set).zip(ti.colList).collect { case (true, col) => col }
    val setStr = setCols.map { col => s"${col.colName} = ${col.?}" }.mkString(", ")

    new QueryI[T[Id]](
      s"${ti.referenceName} - update",
      fr"UPDATE ${ti.referenceName} $name SET $setStr WHERE ${AppliedCol(name, ti.colInfo.id).ref} = ${ti.colInfo.id.?}" ## QueryInputMapper.id,
      (
        RowEncoder.forTable(ti.functorK.mapK(ti.colInfo) { [a] => (c: Col[a]) => c.colCodec.encoder }, set) ~
          RowEncoder.fromColEncoder(ti.colInfo.id.colCodec.encoder.asInstanceOf[ColEncoder[Any]])
      ).cmap[T[Id]] { t => (t, t.id) },
    )
  }

}
