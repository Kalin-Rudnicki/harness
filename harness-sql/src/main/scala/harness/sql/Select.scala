package harness.sql

export Select.Return.given
import harness.core.Zip
import harness.sql.typeclass.*
import scala.annotation.targetName
import shapeless3.deriving.*

object Select {

  final case class Return[T] private[Select] (
      columns: List[ColRef],
      rowDecoder: RowDecoder[T],
  ) {

    def ~[T2](that: Return[T2])(implicit zip: Zip[T, T2]): Return[zip.Out] =
      Return(this.columns ::: that.columns, this.rowDecoder ~ that.rowDecoder)

  }
  object Return {

    @targetName("convertTable")
    given [T[_[_]] <: Table](using ti: TableInfo[T]): Conversion[T[AppliedCol], Return[T[Id]]] =
      t => Return(ti.tableCols.columns(t), ti.rowCodec.decoder)

    @targetName("convertOptTable")
    given [T[_[_]] <: Table](using ti: TableInfo[T]): Conversion[T[AppliedCol.Opt], Return[Option[T[Id]]]] =
      _ => throw new RuntimeException("TODO : implement (RowDecoder/RowEncoder).optional")

    @targetName("convertCol")
    given [T]: Conversion[AppliedCol[T], Return[T]] =
      t => Return(List(t.ref), RowDecoder.fromColDecoder(t.col.colCodec.decoder))

    @targetName("convertOptCol")
    given [T]: Conversion[AppliedCol.Opt[T], Return[T]] =
      t => throw new RuntimeException("TODO : implement (RowDecoder/RowEncoder).optional")

  }

  final class Q1[T] private[Select] (t: T, query: String, queryInputMapper: QueryInputMapper) {

    // TODO (KR) : REMOVE
    println()
    println(this)

    def join[T2[_[_]] <: Table](name: String)(implicit t2ti: TableInfo[T2], zip: Zip[T, T2[AppliedCol]]): AppliedQ1[zip.Out] =
      AppliedQ1(
        zip.zip(t, t2ti.functorK.mapK(t2ti.colInfo)(AppliedCol.withVarName(name))),
        s"$query JOIN ${t2ti.tableName} $name",
        queryInputMapper,
      )

    def leftJoin[T2[_[_]] <: Table](name: String)(implicit t2ti: TableInfo[T2], zip: Zip[T, T2[AppliedCol.Opt]]): AppliedQ1[zip.Out] =
      AppliedQ1(
        zip.zip(t, t2ti.functorK.mapK(t2ti.functorK.mapK(t2ti.colInfo)(AppliedCol.withVarName(name)))(AppliedCol.optional)),
        s"$query LEFT JOIN ${t2ti.tableName} $name",
        queryInputMapper,
      )

    def where(f: T => QueryBool): Q2[T] = {
      val qb = f(t)
      Q2(
        t,
        s"$query WHERE ${qb.wrapped}",
        queryInputMapper + qb.queryInputMapper,
      )
    }

    def select[T2](f: T => Return[T2]): Q3[T2] = {
      val ret = f(t)
      Q3(s"SELECT ${ret.columns.mkString(", ")} FROM $query", ret, queryInputMapper)
    }

    override def toString: String = s"Q1:\n  - $t\n  - $query"

  }

  final class AppliedQ1[T] private[Select] (t: T, query: String, queryInputMapper: QueryInputMapper) {

    // TODO (KR) : REMOVE
    println()
    println(this)

    def on(f: T => QueryBool): Q1[T] = {
      val qb = f(t)
      Q1(
        t,
        s"$query ON ${qb.wrapped}",
        queryInputMapper + qb.queryInputMapper,
      )
    }

    override def toString: String = s"AppliedQ1:\n  - $t\n  - $query"

  }

  final class Q2[T] private[Select] (t: T, query: String, queryInputMapper: QueryInputMapper) {

    // TODO (KR) : REMOVE
    println()
    println(this)

    def select[T2](f: T => Return[T2]): Q3[T2] = {
      val ret = f(t)
      Q3(s"SELECT ${ret.columns.mkString(", ")} FROM $query", ret, queryInputMapper)
    }

    override def toString: String = s"Q2:\n  - $t\n  - $query"

  }

  final class Q3[T] private[Select] (query: String, ret: Return[T], queryInputMapper: QueryInputMapper) {

    // TODO (KR) : REMOVE
    println()
    println(this)

    override def toString: String = query

  }

  def from[T[_[_]] <: Table](name: String)(implicit ti: TableInfo[T]): Q1[T[AppliedCol]] =
    Q1(
      ti.functorK.mapK(ti.colInfo)(AppliedCol.withVarName(name)),
      s"${ti.tableName} $name",
      QueryInputMapper.empty,
    )

}
