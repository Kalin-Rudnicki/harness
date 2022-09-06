package harness.sql.query

import harness.sql.*
import harness.sql.typeclass.*
import shapeless3.deriving.Id

object Delete {

  def from[T[_[_]] <: Table](name: String)(implicit ti: TableSchema[T]): Q1[T[AppliedCol]] =
    Q1(
      ti.functorK.mapK(ti.colInfo)(AppliedCol.withVarName(name)),
      s"DELETE FROM ${ti.tableName} $name",
    )

  final class Q1[T] private[Delete] (
      t: T,
      query: String,
  ) {

    // TODO (KR) : Support joins

    def where(f: T => QueryBool): Delete.Query[T] = {
      val qb = f(t)
      Delete.Query(
        t,
        s"$query WHERE ${qb.wrapped}",
        qb.queryInputMapper,
      )
    }

  }

  final class Query[T] private[Delete] (
      t: T,
      private[query] val query: String,
      private[query] val queryInputMapper: QueryInputMapper,
  ) {

    def returning[T2](f: T => Returning[T2]): QueryR[T2] = {
      val ret = f(t)
      QueryR(
        s"$query RETURNING ${ret.columns.mkString(", ")}",
        ret.rowDecoder,
        queryInputMapper,
      )
    }

  }

  final class QueryR[O] private[Delete] (
      private[query] val query: String,
      private[query] val decoder: RowDecoder[O],
      private[query] val queryInputMapper: QueryInputMapper,
  )

}
