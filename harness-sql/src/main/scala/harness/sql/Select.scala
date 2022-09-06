package harness.sql

export Select.Return.given
import harness.core.Zip
import shapeless3.deriving.*

object Select {

  // TODO (KR) : MOVE?
  def optional: AppliedCol ~> AppliedCol.Opt =
    [a] => (fa: AppliedCol[a]) => fa.optional

  final class Return[T] private[Select] (val columns: List[ColRef]) {

    def ~[T2](that: Return[T2])(implicit zip: Zip[T, T2]): Return[zip.Out] =
      Return(this.columns ::: that.columns)

  }
  object Return {

    given [T[_[_]] <: Table](using ti: TableInfo[T]): Conversion[T[AppliedCol], Return[T[cats.Id]]] =
      t => Return(ti.tableCols.columns(t))
    // given [T[_[_]] <: Table](using ti: TableInfo[T]): Conversion[T[AppliedCol.Opt], Return[T[cats.Id]]] =
    //   t => Return(ti.columns(t))
    given [T]: Conversion[AppliedCol[T], Return[T]] =
      t => Return(List(t.ref))

  }

  final class Q1[T] private[Select] (t: T, query: String) {

    // TODO (KR) : REMOVE
    println()
    println(this)

    def join[T2[_[_]] <: Table](name: String)(implicit t2ti: TableInfo[T2], zip: Zip[T, T2[AppliedCol]]): AppliedQ1[zip.Out] =
      AppliedQ1(
        zip.zip(t, t2ti.functorK.mapK(t2ti.colInfo)(AppliedCol.withVarName(name))),
        s"$query JOIN ${t2ti.tableName} $name",
      )

    def leftJoin[T2[_[_]] <: Table](name: String)(implicit t2ti: TableInfo[T2], zip: Zip[T, T2[AppliedCol.Opt]]): AppliedQ1[zip.Out] =
      AppliedQ1(
        zip.zip(t, t2ti.functorK.mapK(t2ti.functorK.mapK(t2ti.colInfo)(AppliedCol.withVarName(name)))(optional)),
        s"$query LEFT JOIN ${t2ti.tableName} $name",
      )

    def where(f: T => QueryBool): Q2[T] =
      Q2(
        t,
        s"$query WHERE ${f(t).wrapped}",
      )

    def select[T2](f: T => Return[T2]): Q3[T2] = {
      val ret = f(t)
      Q3(s"SELECT ${ret.columns.mkString(", ")} FROM $query", ret)
    }

    override def toString: String = s"Q1:\n  - $t\n  - $query"

  }

  final class AppliedQ1[T] private[Select] (t: T, query: String) {

    // TODO (KR) : REMOVE
    println()
    println(this)

    def on(f: T => QueryBool): Q1[T] =
      Q1(
        t,
        s"$query ON ${f(t).wrapped}",
      )

    override def toString: String = s"AppliedQ1:\n  - $t\n  - $query"

  }

  final class Q2[T] private[Select] (t: T, query: String) {

    // TODO (KR) : REMOVE
    println()
    println(this)

    def select[T2](f: T => Return[T2]): Q3[T2] = {
      val ret = f(t)
      Q3(s"SELECT ${ret.columns.mkString(", ")} FROM $query", ret)
    }

    override def toString: String = s"Q2:\n  - $t\n  - $query"

  }

  final class Q3[T] private[Select] (query: String, ret: Return[T]) {

    // TODO (KR) : REMOVE
    println()
    println(this)

    override def toString: String = query

  }

  def from[T[_[_]] <: Table](name: String)(implicit ti: TableInfo[T]): Q1[T[AppliedCol]] =
    Q1(
      ti.functorK.mapK(ti.colInfo)(AppliedCol.withVarName(name)),
      s"${ti.tableName} $name",
    )

}
