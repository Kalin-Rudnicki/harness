package harness.sql.query

import harness.sql.*
import harness.sql.typeclass.*

object Delete {

  def from[T[_[_]] <: Table](name: String)(implicit ti: TableSchema[T]): Q1[T[AppliedCol]] =
    Q1(
      ti.functorK.mapK(ti.colInfo)(AppliedCol.withVarName(name)),
      fr"DELETE FROM ${ti.referenceName} $name",
    )

  final class Q1[T] private[Delete] (
      t: T,
      fragment: Fragment,
  ) {

    // TODO (KR) : Support joins

    def where(f: T => QueryBool): Delete.Query[T] =
      Delete.Query(
        t,
        fr"$fragment WHERE ${f(t)}",
      )

  }

  final class Query[T] private[Delete] (
      t: T,
      private[query] val fragment: Fragment,
  ) {

    def returning[T2](f: T => Returning[T2]): QueryR[T2] = {
      val ret = f(t)
      QueryR(
        fr"$fragment RETURNING $ret",
        ret.rowDecoder,
      )
    }

  }

  final class QueryR[O] private[Delete] (
      private[query] val fragment: Fragment,
      private[query] val decoder: RowDecoder[O],
  )

}
