package harness.sql.query

import harness.sql.*
import harness.sql.typeclass.*
import shapeless3.deriving.Id

object Update {

  def apply[T[_[_]] <: Table](name: String)(implicit ti: TableSchema[T]): Q1[T[AppliedCol]] =
    Q1(
      ti.functorK.mapK(ti.colInfo)(AppliedCol.withVarName(name)),
      s"${ti.tableName} $name",
    )

  final class Q1[T] private[Update] (
      t: T,
      tableName: String,
  ) {

    // TODO (KR) : Support joins

    // TODO (KR) : Add 'set' without 'where'

    def where(f: T => QueryBool): Q2[T] = {
      val qb = f(t)
      Q2(
        t,
        tableName,
        qb.wrapped,
        qb.queryInputMapper,
      )
    }

  }

  final class Q2[T] private[Update] (
      t: T,
      tableName: String,
      whereClause: String,
      queryInputMapper: QueryInputMapper,
  ) {

    def set(f: T => QuerySet): Update.Query[T] = {
      val qs = f(t)
      Update.Query(
        t,
        s"UPDATE $tableName SET ${qs.wrapped} WHERE $whereClause",
        qs.queryInputMapper + this.queryInputMapper,
      )
    }

  }

  final class Query[T] private[Update] (
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

  final class QueryR[O] private[Update] (
      private[query] val query: String,
      private[query] val decoder: RowDecoder[O],
      private[query] val queryInputMapper: QueryInputMapper,
  )

}
