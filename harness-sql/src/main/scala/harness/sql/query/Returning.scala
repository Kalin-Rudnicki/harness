package harness.sql.query

import harness.core.Zip
import harness.sql.*
import harness.sql.typeclass.*
import shapeless3.deriving.Id

final case class Returning[T] private (
    private[sql] val selects: List[String],
    private[sql] val rowDecoder: RowDecoder[T],
    private[sql] val qim: QueryInputMapper,
) {

  def ~[T2](that: Returning[T2])(implicit z: Zip[T, T2]): Returning[z.Out] =
    Returning(this.selects ::: that.selects, this.rowDecoder ~ that.rowDecoder, this.qim + that.qim)

}
object Returning {

  given convertTable[T[_[_]] <: Table](using ti: TableSchema[T]): Conversion[T[AppliedCol], Returning[T[Id]]] =
    t => Returning(ti.tableCols.columns(t).map(_.toString), ti.rowCodec.decoder, QueryInputMapper.empty)

  given convertOptTable[T[_[_]] <: Table](using ti: TableSchema[T]): Conversion[T[AppliedCol.Opt], Returning[Option[T[Id]]]] =
    t =>
      Returning(
        ti.tableCols.columns(ti.functorK.mapK(t) { [a] => (aco: AppliedCol.Opt[a]) => aco.wrapped }).map(_.toString),
        ti.rowCodec.decoder.optional,
        QueryInputMapper.empty,
      )

  given convertCol[T]: Conversion[AppliedCol[T], Returning[T]] =
    t => Returning(List(t.ref.toString), RowDecoder.fromCol(t.col), QueryInputMapper.empty)

  // TODO (KR) : use low-priority to have `AppliedCol.Opt[Option[Int]]` decode to `Option[Int]` instead of `Option[Option[Int]]`
  given convertOptCol[T]: Conversion[AppliedCol.Opt[T], Returning[Option[T]]] =
    t => Returning(List(t.wrapped.ref.toString), RowDecoder.fromCol(t.wrapped.col.optional), QueryInputMapper.empty)

  given convertReturningJson[T]: Conversion[Select.Query[T] & Select.JsonReturn, Returning[T]] =
    t => Returning(List(s"(${t.fragment.sql})"), t.decoder, t.fragment.qim)

}

export harness.sql.query.Returning.given
