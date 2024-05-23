package harness.sql.query

import harness.core.Zip
import harness.deriving.*
import harness.sql.*
import harness.sql.typeclass.*

final case class Returning[T] private (
    private[sql] val fragment: Fragment,
    private[sql] val decoder: QueryDecoderMany[T],
) {

  def ~[T2](that: Returning[T2])(implicit z: Zip[T, T2]): Returning[z.Out] =
    Returning(fr"${this.fragment}, ${that.fragment}", this.decoder ~ that.decoder)

}
object Returning {

  given convertTable[T[_[_]] <: Table](using ti: TableSchema[T]): Conversion[T[AppliedCol], Returning[T[K11.Identity]]] =
    t =>
      Returning(
        Fragment.sql(ti.flatten(t).map(_.show).mkString(", ")),
        ti.codec.decoder,
      )

  given convertOptTable[T[_[_]] <: Table](using ti: TableSchema[T]): Conversion[T[AppliedCol.Opt], Returning[Option[T[K11.Identity]]]] =
    t =>
      Returning(
        Fragment.sql(ti.flatten(t).map(_.wrapped.show).mkString(", ")),
        ti.codec.decoder.optional,
      )

  given convertCol[T]: Conversion[AppliedCol[T], Returning[T]] =
    t => Returning(fr"$t", t.col.codec.decoder)

  // TODO (KR) : use low-priority to have `AppliedCol.Opt[Option[Int]]` decode to `Option[Int]` instead of `Option[Option[Int]]`
  given convertOptCol[T]: Conversion[AppliedCol.Opt[T], Returning[Option[T]]] =
    t => Returning(fr"$t", QueryDecoderMany.fromCol(t.wrapped.col.optional))

  given convertReturningJson[T]: Conversion[Select.Query[T] & Select.JsonReturn, Returning[T]] =
    t => Returning(t.fragment.wrapInParens, t.decoder)

}

export harness.sql.query.Returning.given
