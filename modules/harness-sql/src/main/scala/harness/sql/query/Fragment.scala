package harness.sql.query

import cats.syntax.option.*
import harness.sql.*
import harness.sql.typeclass.QueryDecoderMany

final case class Fragment(
    sql: String,
    qim: QueryInputMapper,
) { self =>

  def mapSql(f: String => String): Fragment = Fragment(f(sql), qim)

  def wrapInParens: Fragment = self.mapSql(sql => s"($sql)")
  def wrapInParensIf(cond: Boolean): Fragment = self.mapSql(sql => if (cond) s"($sql)" else sql)

  def +:(str: String): Fragment = self.mapSql(sql => s"$str$sql")
  def :+(str: String): Fragment = self.mapSql(sql => s"$sql$str")

  def toQuery(queryName: String): Query = Query(queryName, self)
  def toQueryO[O](queryName: String)(using decoder: QueryDecoderMany[O]): QueryO[O] = QueryO(queryName, self, decoder)

}
object Fragment {

  type Arg = String | Fragment | ColRef | QueryBool | QuerySet | AppliedCol[?] | AppliedCol.Opt[?] | Returning[?]

  val empty: Fragment = Fragment("", QueryInputMapper.Empty)

  def sql(sql: String): Fragment =
    Fragment(sql, QueryInputMapper.Empty)

  def joinAll(frags: Iterable[Fragment], start: String, sep: String, end: String): Fragment =
    Fragment(
      frags.map(_.sql).mkString(start, sep, end),
      frags.map(_.qim).foldLeft(QueryInputMapper.Empty: QueryInputMapper) { _ + _ },
    )
  def joinAll(frags: Iterable[Fragment], sep: String): Fragment =
    joinAll(frags, "", sep, "")
  def joinAll(frags: Iterable[Fragment]): Fragment =
    joinAll(frags, "", "", "")

}

extension (sc: StringContext) {
  def fr(args: Fragment.Arg*): Fragment = {
    def toTuple(arg: Fragment.Arg): (String, QueryInputMapper) =
      arg match {
        case str: String                      => (str, QueryInputMapper.Empty)
        case fragment: Fragment               => (fragment.sql, fragment.qim)
        case queryBool: QueryBool             => (queryBool.fragment.sql, queryBool.fragment.qim)
        case querySet: QuerySet               => (querySet.fragment.sql, querySet.fragment.qim)
        case colRef: ColRef                   => (colRef.show, QueryInputMapper.Empty)
        case appliedCol: AppliedCol[?]        => (appliedCol.ref.show, QueryInputMapper.Empty)
        case appliedColOpt: AppliedCol.Opt[?] => (appliedColOpt.wrapped.ref.show, QueryInputMapper.Empty)
        case returning: Returning[?]          => (returning.fragment.sql, returning.fragment.qim)
      }

    def join(string: String, arg: Fragment.Arg): Fragment = {
      val (sqlStr, qim) = toTuple(arg)
      Fragment(s"$string$sqlStr", qim)
    }

    (sc.parts.toList, args.toList) match {
      case (List(str), Nil) =>
        Fragment.sql(str)
      case (List(str1, str2), List(arg)) =>
        val (sqlStr, qim) = toTuple(arg)
        Fragment(s"$str1$sqlStr$str2", qim)
      case (List(str1, str2, str3), List(arg1, arg2)) =>
        val (sqlStr1, qim1) = toTuple(arg1)
        val (sqlStr2, qim2) = toTuple(arg2)
        Fragment(s"$str1$sqlStr1$str2$sqlStr2$str3", qim1 + qim2)
      case (partsList, argsList) =>
        val plr = partsList.reverse
        Fragment.joinAll((Fragment.sql(plr.head) :: plr.tail.zip(argsList.reverse).map(join)).reverse)
    }
  }
}
