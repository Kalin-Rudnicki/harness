package harness.sql.query

import harness.sql.*
import harness.sql.typeclass.*
import shapeless3.deriving.Id
import zio.Chunk
import zio.json.JsonDecoder

object Select {

  def from[T[_[_]] <: Table](name: String)(implicit ti: TableSchema[T]): Q1[T[AppliedCol]] =
    Q1(
      ti.functorK.mapK(ti.colInfo)(AppliedCol.withVarName(name)),
      s"${ti.referenceName} $name",
      QueryInputMapper.empty,
    )

  final class Q1[T] private[Select] (t: T, query: String, queryInputMapper: QueryInputMapper) {

    def join[T2[_[_]] <: Table](name: String)(implicit t2ti: TableSchema[T2], z: ZipCodec[T, T2[AppliedCol]]): Q2[z.C] =
      Q2(
        z.zip(t, t2ti.functorK.mapK(t2ti.colInfo)(AppliedCol.withVarName(name))),
        s"$query JOIN ${t2ti.referenceName} $name",
        queryInputMapper,
      )

    def leftJoin[T2[_[_]] <: Table](name: String)(implicit t2ti: TableSchema[T2], z: ZipCodec[T, T2[AppliedCol.Opt]]): Q2[z.C] =
      Q2(
        z.zip(t, t2ti.functorK.mapK(t2ti.functorK.mapK(t2ti.colInfo)(AppliedCol.withVarName(name)))(AppliedCol.optional)),
        s"$query LEFT JOIN ${t2ti.referenceName} $name",
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

    def orderBy[C](f: T => AppliedCol[C]): Q4[T] =
      Q4(
        t,
        s"$query ORDER BY ${f(t).ref}",
        queryInputMapper,
      )

    def returning[T2](f: T => Returning[T2]): Select.Query[T2] = {
      val ret = f(t)
      Select.Query(s"SELECT ${ret.selects.mkString(", ")} FROM $query", ret.rowDecoder, queryInputMapper + ret.qim)
    }

    def returningJson[T2](f: T => ReturningJson[T2]): Select.Q5[T2] = {
      val ret = f(t)
      Select.Q5(ret.selectStr, query, ret.decoder, queryInputMapper)
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

    def orderBy[C](f: T => AppliedCol[C]): Q4[T] =
      Q4(
        t,
        s"$query ORDER BY ${f(t).ref}",
        queryInputMapper,
      )

    def returning[T2](f: T => Returning[T2]): Select.Query[T2] = {
      val ret = f(t)
      Select.Query(s"SELECT ${ret.selects.mkString(", ")} FROM $query", ret.rowDecoder, queryInputMapper)
    }

    def returningJson[T2](f: T => ReturningJson[T2]): Select.Q5[T2] = {
      val ret = f(t)
      Select.Q5(ret.selectStr, query, ret.decoder, queryInputMapper)
    }

  }

  final class Q4[T] private[Select] (t: T, query: String, queryInputMapper: QueryInputMapper) {

    def returning[T2](f: T => Returning[T2]): Select.Query[T2] = {
      val ret = f(t)
      Select.Query(s"SELECT ${ret.selects.mkString(", ")} FROM $query", ret.rowDecoder, queryInputMapper)
    }

    def returningJson[T2](f: T => ReturningJson[T2]): Select.Q5[T2] = {
      val ret = f(t)
      Select.Q5(ret.selectStr, query, ret.decoder, queryInputMapper)
    }

  }

  final class Q5[O] private[Select] (
      select: String,
      query: String,
      decoder: JsonDecoder[O],
      queryInputMapper: QueryInputMapper,
  ) {

    def single: Query[O] with JsonReturn =
      Select.Query(s"SELECT $select FROM $query", RowDecoder.fromColDecoder(ColDecoder.json[O](decoder)), queryInputMapper).asInstanceOf[Select.Query[O] with JsonReturn]
    def option: Query[Option[O]] with JsonReturn =
      Select.Query(s"SELECT $select FROM $query", RowDecoder.fromColDecoder(ColDecoder.json[O](decoder)).optional, queryInputMapper).asInstanceOf[Select.Query[Option[O]] with JsonReturn]
    def chunk: Query[Chunk[O]] with JsonReturn =
      Select
        .Query(
          s"COALESCE((SELECT json_agg($select) FROM $query), '[]' :: JSON)",
          RowDecoder.fromColDecoder(ColDecoder.json[Chunk[O]](JsonDecoder.chunk[O](decoder))),
          queryInputMapper,
        )
        .asInstanceOf[Select.Query[Chunk[O]] with JsonReturn]

  }

  // =====|  |=====

  final class Query[O] private[Select] (
      private[query] val query: String,
      private[query] val decoder: RowDecoder[O],
      private[query] val queryInputMapper: QueryInputMapper,
  )

  type JsonReturn

}
