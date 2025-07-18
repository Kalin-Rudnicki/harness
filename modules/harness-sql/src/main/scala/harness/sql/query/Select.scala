package harness.sql.query

import harness.core.Zip
import harness.sql.*
import harness.sql.typeclass.*
import zio.Chunk
import zio.json.JsonDecoder

object Select {

  def from[T[_[_]] <: Table](name: String)(implicit ti: TableSchema[T]): Q1[T[AppliedCol]] =
    Q1(
      ti.functorK.mapK(ti.columns)(AppliedCol.withVarName(name)),
      fr"${ti.referenceName} $name",
    )

  final class Q1[T] private[Select] (t: T, fragment: Fragment) {

    def join[T2[_[_]] <: Table](name: String)(implicit t2ti: TableSchema[T2], z: Zip[T, T2[AppliedCol]]): Q2[z.Out] =
      Q2(
        z.zip(t, t2ti.functorK.mapK(t2ti.columns)(AppliedCol.withVarName(name))),
        fr"$fragment JOIN ${t2ti.referenceName} $name",
      )

    def leftJoin[T2[_[_]] <: Table](name: String)(implicit t2ti: TableSchema[T2], z: Zip[T, T2[AppliedCol.Opt]]): Q2[z.Out] =
      Q2(
        z.zip(t, t2ti.functorK.mapK(t2ti.functorK.mapK(t2ti.columns)(AppliedCol.withVarName(name)))(AppliedCol.optional)),
        fr"$fragment LEFT JOIN ${t2ti.referenceName} $name",
      )

    def where(f: T => QueryBool): Q3[T] =
      Q3(
        t,
        fr"$fragment WHERE ${f(t)}",
      )

    // TODO (KR) : improve
    def orderBy[C](f: T => AppliedCol[C]): Q4[T] =
      Q4(
        t,
        fr"$fragment ORDER BY ${f(t).ref}",
      )

    def returning[T2](f: T => Returning[T2]): Select.Query[T2] = {
      val ret = f(t)
      Select.Query(fr"SELECT $ret FROM $fragment", ret.decoder)
    }

    def returningJson[T2](f: T => ReturningJson[T2]): Select.Q5[T2] = {
      val ret = f(t)
      Select.Q5(Fragment.sql(ret.selectStr), fragment, ret.decoder)
    }

  }

  final class Q2[T] private[Select] (t: T, fragment: Fragment) {

    def on(f: T => QueryBool): Q1[T] =
      Q1(
        t,
        fr"$fragment ON ${f(t)}",
      )

  }

  final class Q3[T] private[Select] (t: T, fragment: Fragment) {

    def orderBy[C](f: T => AppliedCol[C]): Q4[T] =
      Q4(
        t,
        fr"$fragment ORDER BY ${f(t).ref}",
      )

    def returning[T2](f: T => Returning[T2]): Select.Query[T2] = {
      val ret = f(t)
      Select.Query(fr"SELECT $ret FROM $fragment", ret.decoder)
    }

    def returningJson[T2](f: T => ReturningJson[T2]): Select.Q5[T2] = {
      val ret = f(t)
      Select.Q5(Fragment.sql(ret.selectStr), fragment, ret.decoder)
    }

  }

  final class Q4[T] private[Select] (t: T, fragment: Fragment) {

    def returning[T2](f: T => Returning[T2]): Select.Query[T2] = {
      val ret = f(t)
      Select.Query(fr"SELECT $ret FROM $fragment", ret.decoder)
    }

    def returningJson[T2](f: T => ReturningJson[T2]): Select.Q5[T2] = {
      val ret = f(t)
      Select.Q5(Fragment.sql(ret.selectStr), fragment, ret.decoder)
    }

  }

  final class Q5[O] private[Select] (
      select: Fragment,
      query: Fragment,
      decoder: JsonDecoder[O],
  ) {

    def single: Query[O] & JsonReturn =
      Select.Query(fr"SELECT $select FROM $query", QueryDecoderMany.fromSingle(QueryDecoderSingle.encodedJson[O](decoder))).asInstanceOf[Select.Query[O] & JsonReturn]
    def option: Query[Option[O]] & JsonReturn =
      Select.Query(fr"SELECT $select FROM $query", QueryDecoderMany.fromSingle(QueryDecoderSingle.encodedJson[O](decoder)).optional).asInstanceOf[Select.Query[Option[O]] & JsonReturn]
    def chunk: Query[Chunk[O]] & JsonReturn =
      Select
        .Query(
          fr"COALESCE((SELECT json_agg($select) FROM $query), '[]' :: JSON)",
          QueryDecoderMany.fromSingle(QueryDecoderSingle.encodedJson[Chunk[O]](JsonDecoder.chunk[O](using decoder))),
        )
        .asInstanceOf[Select.Query[Chunk[O]] & JsonReturn]

  }

  // =====|  |=====

  final class Query[O] private[Select] (
      private[query] val fragment: Fragment,
      private[query] val decoder: QueryDecoderMany[O],
  )

  type JsonReturn

}
