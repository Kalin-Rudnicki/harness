package harness.sql.query

import harness.sql.*
import harness.sql.typeclass.*

object Update {

  def apply[T[_[_]] <: Table](name: String)(implicit ti: TableSchema[T]): Q1[T[AppliedCol]] =
    Q1(
      ti.functorK.mapK(ti.columns)(AppliedCol.withVarName(name)),
      fr"${ti.referenceName} $name",
    )

  final class Q1[T] private[Update] (
      t: T,
      tableName: Fragment,
  ) {

    // TODO (KR) : Support joins

    // TODO (KR) : Add 'set' without 'where'

    def where(f: T => QueryBool): Q2[T] =
      Q2(
        t,
        tableName,
        f(t).fragment,
      )

  }

  final class Q2[T] private[Update] (
      t: T,
      tableName: Fragment,
      whereClause: Fragment,
  ) {

    def set(f: T => QuerySet): Update.Query[T] =
      Update.Query(
        t,
        fr"UPDATE $tableName SET ${f(t)} WHERE $whereClause",
      )

  }

  final class Query[T] private[Update] (
      t: T,
      private[query] val fragment: Fragment,
  ) {

    def returning[T2](f: T => Returning[T2]): QueryR[T2] = {
      val ret = f(t)
      QueryR(
        fr"$fragment RETURNING $ret",
        ret.decoder,
      )
    }

  }

  final class QueryR[O] private[Update] (
      private[query] val fragment: Fragment,
      private[query] val decoder: QueryDecoderMany[O],
  )

}
