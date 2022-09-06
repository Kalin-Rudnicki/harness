package harness.sql.query

import harness.sql.*
import harness.sql.typeclass.*
import shapeless3.deriving.Id

final case class Returning[T] private (
    private[sql] val columns: List[ColRef],
    private[sql] val rowDecoder: RowDecoder[T],
) {

  def ~[T2](that: Returning[T2])(implicit z: ZipCodec[T, T2]): Returning[z.C] =
    Returning(this.columns ::: that.columns, this.rowDecoder ~ that.rowDecoder)

}
object Returning {

  given convertTable[T[_[_]] <: Table](using ti: TableSchema[T]): Conversion[T[AppliedCol], Returning[T[Id]]] =
    t => Returning(ti.tableCols.columns(t), ti.rowCodec.decoder)

  given convertOptTable[T[_[_]] <: Table](using ti: TableSchema[T]): Conversion[T[AppliedCol.Opt], Returning[Option[T[Id]]]] =
    t => Returning(ti.tableCols.columns(ti.functorK.mapK(t) { [a] => (aco: AppliedCol.Opt[a]) => aco.wrapped }), ti.rowCodec.decoder.optional)

  given convertCol[T]: Conversion[AppliedCol[T], Returning[T]] =
    t => Returning(List(t.ref), RowDecoder.fromColDecoder(t.col.colCodec.decoder))

  // TODO (KR) : use low-priority to have `AppliedCol.Opt[Option[Int]]` decode to `Option[Int]` instead of `Option[Option[Int]]`
  given convertOptCol[T]: Conversion[AppliedCol.Opt[T], Returning[Option[T]]] =
    t => Returning(List(t.wrapped.ref), RowDecoder.fromColDecoder(t.wrapped.col.colCodec.decoder.optional))

}

export harness.sql.query.Returning.given
