package harness.sql.query

import harness.core.Zip
import harness.sql.*
import harness.sql.typeclass.*
import scala.annotation.targetName
import shapeless3.deriving.Id

object Select {

  def from[T[_[_]] <: Table](name: String)(implicit ti: TableInfo[T]): Q1[T[AppliedCol]] =
    Q1(
      ti.functorK.mapK(ti.colInfo)(AppliedCol.withVarName(name)),
      s"${ti.tableName} $name",
      QueryInputMapper.empty,
    )

  final class Q1[T] private[Select] (t: T, query: String, queryInputMapper: QueryInputMapper) {

    def join[T2[_[_]] <: Table](name: String)(implicit t2ti: TableInfo[T2], zip: Zip[T, T2[AppliedCol]]): Q2[zip.Out] =
      Q2(
        zip.zip(t, t2ti.functorK.mapK(t2ti.colInfo)(AppliedCol.withVarName(name))),
        s"$query JOIN ${t2ti.tableName} $name",
        queryInputMapper,
      )

    def leftJoin[T2[_[_]] <: Table](name: String)(implicit t2ti: TableInfo[T2], zip: Zip[T, T2[AppliedCol.Opt]]): Q2[zip.Out] =
      Q2(
        zip.zip(t, t2ti.functorK.mapK(t2ti.functorK.mapK(t2ti.colInfo)(AppliedCol.withVarName(name)))(AppliedCol.optional)),
        s"$query LEFT JOIN ${t2ti.tableName} $name",
        queryInputMapper,
      )

    def where(f: T => QueryBool): Q3[T] = {
      val qb = f(t)
      Q3(
        t,
        s"$query WHERE ${qb.wrapped}",
        queryInputMapper + qb.queryInputMapper,
      )
    }

    def returning[T2](f: T => Returning[T2]): Query[T2] = {
      val ret = f(t)
      Query(s"SELECT ${ret.columns.mkString(", ")} FROM $query", ret.rowDecoder, queryInputMapper)
    }

  }

  final class Q2[T] private[Select] (t: T, query: String, queryInputMapper: QueryInputMapper) {

    def on(f: T => QueryBool): Q1[T] = {
      val qb = f(t)
      Q1(
        t,
        s"$query ON ${qb.wrapped}",
        queryInputMapper + qb.queryInputMapper,
      )
    }

  }

  final class Q3[T] private[Select] (t: T, query: String, queryInputMapper: QueryInputMapper) {

    def returning[T2](f: T => Returning[T2]): Query[T2] = {
      val ret = f(t)
      Query(s"SELECT ${ret.columns.mkString(", ")} FROM $query", ret.rowDecoder, queryInputMapper)
    }

  }

  final class Query[O] private[Select] (
      private[query] val query: String,
      private[query] val decoder: RowDecoder[O],
      private[query] val queryInputMapper: QueryInputMapper,
  )

}
