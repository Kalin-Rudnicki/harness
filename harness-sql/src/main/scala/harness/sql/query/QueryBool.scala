package harness.sql.query

import cats.syntax.option.*
import harness.sql.*
import scala.util.NotGiven

final case class QueryBool private[sql] (
    private[sql] val wrapped: String,
    private[sql] val unaryNeedsParens: Boolean,
    private[sql] val binaryNeedsParens: Boolean,
    private[sql] val queryInputMapper: QueryInputMapper,
) {

  def unaryString: String = if (unaryNeedsParens) s"($wrapped)" else wrapped
  def binaryString: String = if (binaryNeedsParens) s"($wrapped)" else wrapped

  def unary_! : QueryBool = QueryBool(s"NOT $unaryString", false, false, queryInputMapper)
  def &&(that: QueryBool): QueryBool =
    QueryBool(
      s"${this.binaryString} AND ${that.binaryString}",
      true,
      true,
      this.queryInputMapper + that.queryInputMapper,
    )
  def ||(that: QueryBool): QueryBool =
    QueryBool(
      s"${this.binaryString} OR ${that.binaryString}",
      true,
      true,
      this.queryInputMapper + that.queryInputMapper,
    )

}

given Conversion[AppliedCol[Boolean], QueryBool] = a => QueryBool(s"${a.tableVarName}.${a.col.colName}", false, false, QueryInputMapper.empty)
given Conversion[AppliedCol.Opt[Boolean], QueryBool] = a => QueryBool(s"${a.wrapped.tableVarName}.${a.wrapped.col.colName}", false, false, QueryInputMapper.empty)

trait QueryBoolOps[A, B] private {
  def build(a: A, b: B, op: String): QueryBool
}
object QueryBoolOps {

  final case class Empty[A, B] private (a: A => ColRef, b: B => ColRef)
  object Empty {

    implicit def col_col[A]: QueryBoolOps.Empty[AppliedCol[A], AppliedCol[A]] = Empty(_.ref, _.ref)
    implicit def col_optCol[A]: QueryBoolOps.Empty[AppliedCol[A], AppliedCol.Opt[A]] = Empty(_.ref, _.wrapped.ref)
    implicit def optCol_col[A]: QueryBoolOps.Empty[AppliedCol.Opt[A], AppliedCol[A]] = Empty(_.wrapped.ref, _.ref)
    implicit def optCol_optCol[A]: QueryBoolOps.Empty[AppliedCol.Opt[A], AppliedCol.Opt[A]] = Empty(_.wrapped.ref, _.wrapped.ref)

    implicit def oColA[A, B](implicit e: Empty[AppliedCol[A], B], ev: NotGiven[OptionEv[A]]): Empty[AppliedCol[Option[A]], B] = Empty(_.ref, e.b)
    implicit def oOptColA[A, B](implicit e: Empty[AppliedCol.Opt[A], B], ev: NotGiven[OptionEv[A]]): Empty[AppliedCol.Opt[Option[A]], B] = Empty(_.wrapped.ref, e.b)
    implicit def oColB[A, B](implicit e: Empty[A, AppliedCol[B]], ev: NotGiven[OptionEv[B]]): Empty[A, AppliedCol[Option[B]]] = Empty(e.a, _.ref)
    implicit def oOptColB[A, B](implicit e: Empty[A, AppliedCol.Opt[B]], ev: NotGiven[OptionEv[B]]): Empty[A, AppliedCol.Opt[Option[B]]] = Empty(e.a, _.wrapped.ref)

  }

  implicit def col_id[A]: QueryBoolOps[AppliedCol[A], QueryInput[A]] =
    (a, b, o) =>
      QueryBool(
        s"${a.ref} $o ?",
        true,
        false,
        QueryInputMapper(_ => 1, (in, out, off) => out(off) = a.col.colCodec.encoder.encodeColumn(in(b.idx).asInstanceOf)),
      )
  implicit def oCol_id[A]: QueryBoolOps[AppliedCol[Option[A]], QueryInput[A]] =
    (a, b, o) =>
      QueryBool(
        s"${a.ref} $o ?",
        true,
        false,
        QueryInputMapper(_ => 1, (in, out, off) => out(off) = a.col.colCodec.encoder.encodeColumn(in(b.idx).some.asInstanceOf)),
      )

  implicit def col_const[A]: QueryBoolOps[AppliedCol[A], Constant[A]] =
    (a, b, o) =>
      QueryBool(
        s"${a.ref} $o ?",
        true,
        false,
        QueryInputMapper(_ => 1, (_, out, off) => out(off) = a.col.colCodec.encoder.encodeColumn(b.value)),
      )
  implicit def oCol_const[A]: QueryBoolOps[AppliedCol[Option[A]], Constant[A]] =
    (a, b, o) =>
      QueryBool(
        s"${a.ref} $o ?",
        true,
        false,
        QueryInputMapper(_ => 1, (_, out, off) => out(off) = a.col.colCodec.encoder.encodeColumn(b.value.some)),
      )

  implicit def optCol_id[A]: QueryBoolOps[AppliedCol.Opt[A], QueryInput[A]] =
    (a, b, o) =>
      QueryBool(
        s"${a.wrapped.ref} $o ?",
        true,
        false,
        QueryInputMapper(_ => 1, (in, out, off) => out(off) = a.wrapped.col.colCodec.encoder.encodeColumn(in(b.idx).asInstanceOf)),
      )
  implicit def oOptCol_id[A]: QueryBoolOps[AppliedCol.Opt[Option[A]], QueryInput[A]] =
    (a, b, o) =>
      QueryBool(
        s"${a.wrapped.ref} $o ?",
        true,
        false,
        QueryInputMapper(_ => 1, (in, out, off) => out(off) = a.wrapped.col.colCodec.encoder.encodeColumn(in(b.idx).some.asInstanceOf)),
      )

  implicit def optCol_const[A]: QueryBoolOps[AppliedCol.Opt[A], Constant[A]] =
    (a, b, o) =>
      QueryBool(
        s"${a.wrapped.ref} $o ?",
        true,
        false,
        QueryInputMapper(_ => 1, (_, out, off) => out(off) = a.wrapped.col.colCodec.encoder.encodeColumn(b.value)),
      )
  implicit def oOptCol_const[A]: QueryBoolOps[AppliedCol.Opt[Option[A]], Constant[A]] =
    (a, b, o) =>
      QueryBool(
        s"${a.wrapped.ref} $o ?",
        true,
        false,
        QueryInputMapper(_ => 1, (_, out, off) => out(off) = a.wrapped.col.colCodec.encoder.encodeColumn(b.value.some)),
      )

  implicit def fromEmpty[A, B](implicit e: QueryBoolOps.Empty[A, B]): QueryBoolOps[A, B] = { (a, b, op) =>
    QueryBool(
      s"${e.a(a)} $op ${e.b(b)}",
      true,
      false,
      QueryInputMapper.empty,
    )
  }

}

extension [A](a: A) {
  def ===[B](b: B)(implicit qbo: QueryBoolOps[A, B]): QueryBool = qbo.build(a, b, "=")
  def !==[B](b: B)(implicit qbo: QueryBoolOps[A, B]): QueryBool = qbo.build(a, b, "!=")
  def <[B](b: B)(implicit qbo: QueryBoolOps[A, B]): QueryBool = qbo.build(a, b, "<")
  def <=[B](b: B)(implicit qbo: QueryBoolOps[A, B]): QueryBool = qbo.build(a, b, "<=")
  def >[B](b: B)(implicit qbo: QueryBoolOps[A, B]): QueryBool = qbo.build(a, b, ">")
  def >=[B](b: B)(implicit qbo: QueryBoolOps[A, B]): QueryBool = qbo.build(a, b, ">=")
}

extension [A](a: AppliedCol[Option[A]]) {
  def isNull: QueryBool = QueryBool(s"${a.ref} IS NULL", true, false, QueryInputMapper.empty)
  def isNotNull: QueryBool = QueryBool(s"${a.ref} IS NOT NULL", true, false, QueryInputMapper.empty)
}

extension [A](a: AppliedCol.Opt[A]) {
  def isNull: QueryBool = QueryBool(s"${a.wrapped.ref} IS NULL", true, false, QueryInputMapper.empty)
  def isNotNull: QueryBool = QueryBool(s"${a.wrapped.ref} IS NOT NULL", true, false, QueryInputMapper.empty)
}

// TODO (KR) : LIKE
// TODO (KR) : IN
