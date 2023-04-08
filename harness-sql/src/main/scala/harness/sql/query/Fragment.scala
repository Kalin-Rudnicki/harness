package harness.sql.query

import cats.syntax.option.*
import harness.sql.*

final case class Fragment(
    sql: String,
    qim: QueryInputMapper,
) { self =>

  def mapSql(f: String => String): Fragment = Fragment(f(sql), qim)

  def wrapInParens: Fragment = self.mapSql(sql => s"($sql)")
  def wrapInParensIf(cond: Boolean): Fragment = self.mapSql(sql => if (cond) s"($sql)" else sql)

  def +:(str: String): Fragment = self.mapSql(sql => s"$str$sql")
  def :+(str: String): Fragment = self.mapSql(sql => s"$sql$str")

  def ##(qim: QueryInputMapper): Fragment = Fragment(sql, qim)

}
object Fragment {

  type Arg = String | Fragment | ColRef | QueryBool | QuerySet | AppliedCol[_] | AppliedCol.Opt[_] | Returning[_]

  def empty: Fragment = Fragment("", QueryInputMapper.empty)

  def fromString(sql: String): Fragment =
    Fragment(sql, QueryInputMapper.empty)

  def joinAll(frags: Iterable[Fragment], joinString: String = ""): Fragment =
    Fragment(
      frags.toList.map(_.sql).mkString(joinString),
      frags.map(_.qim).foldLeft(QueryInputMapper.empty) { _ + _ },
    )

}

extension (sc: StringContext) {
  def fr(args: Fragment.Arg*): Fragment = {
    def toTuple(arg: Fragment.Arg): (String, Option[QueryInputMapper]) =
      arg match {
        case str: String                      => (str, None)
        case fragment: Fragment               => (fragment.sql, fragment.qim.some)
        case queryBool: QueryBool             => (queryBool.fragment.sql, queryBool.fragment.qim.some)
        case querySet: QuerySet               => (querySet.fragment.sql, querySet.fragment.qim.some)
        case colRef: ColRef                   => (colRef.toStringNoType, None)
        case appliedCol: AppliedCol[_]        => (appliedCol.ref.toStringNoType, None)
        case appliedColOpt: AppliedCol.Opt[_] => (appliedColOpt.wrapped.ref.toStringNoType, None)
        case returning: Returning[_]          => (returning.selects.mkString(", "), returning.qim.some)
      }

    def join(string: String, arg: Fragment.Arg): Fragment = {
      val (sqlStr, qim) = toTuple(arg)
      Fragment(s"$string$sqlStr", qim.getOrElse(QueryInputMapper.empty))
    }

    (sc.parts.toList, args.toList) match {
      case (List(str), Nil) =>
        Fragment.fromString(str)
      case (List(str1, str2), List(arg)) =>
        val (sqlStr, qim) = toTuple(arg)
        Fragment(s"$str1$sqlStr$str2", qim.getOrElse(QueryInputMapper.empty))
      case (partsList, argsList) =>
        val plr = partsList.reverse
        Fragment.joinAll((Fragment.fromString(plr.head) :: plr.tail.zip(argsList.reverse).map(join)).reverse)
    }
  }
}
