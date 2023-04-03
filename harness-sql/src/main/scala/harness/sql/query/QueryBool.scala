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

  final case class CodeElement[EvT, ObjT, CompareT] private (getObj: (EvT, IArray[Object]) => ObjT)
  object CodeElement {

    implicit def queryInput[T](implicit notOpt: NotGiven[OptionEv[T]]): CodeElement[QueryInput[T], T, T] = CodeElement { (obj, in) => in(obj.idx).asInstanceOf[T] }
    implicit def optQueryInput[T](implicit notOpt: NotGiven[OptionEv[T]]): CodeElement[QueryInput[Option[T]], Option[T], T] = CodeElement { (obj, in) => in(obj.idx).asInstanceOf[Option[T]] }

    implicit def const[T](implicit notOpt: NotGiven[OptionEv[T]]): CodeElement[Constant[T], T, T] = CodeElement { (obj, _) => obj.value }
    implicit def optConst[T](implicit notOpt: NotGiven[OptionEv[T]]): CodeElement[Constant[Option[T]], Option[T], T] = CodeElement { (obj, _) => obj.value }

  }

  final case class QueryElement[EvT, ObjT, CompareT] private (getCol: EvT => AppliedCol[ObjT])
  object QueryElement {

    implicit def appliedCol[T](implicit notOpt: NotGiven[OptionEv[T]]): QueryElement[AppliedCol[T], T, T] = QueryElement(identity)
    implicit def optAppliedCol[T](implicit notOpt: NotGiven[OptionEv[T]]): QueryElement[AppliedCol[Option[T]], Option[T], T] = QueryElement(identity)

    implicit def appliedColOpt[T](implicit notOpt: NotGiven[OptionEv[T]]): QueryElement[AppliedCol.Opt[T], T, T] = QueryElement(_.wrapped)
    implicit def optAppliedColOpt[T](implicit notOpt: NotGiven[OptionEv[T]]): QueryElement[AppliedCol.Opt[Option[T]], Option[T], T] = QueryElement(_.wrapped)

  }

  final case class MapInput[In, Out] private (f: In => Out)
  object MapInput {

    implicit def id[T]: MapInput[T, T] = MapInput(identity)
    implicit def opt[T]: MapInput[T, Option[T]] = MapInput(_.some)

  }

  implicit def queryElement_queryElement[CompareT, AObjT, BObjT, A, B](implicit
      aEv: QueryElement[A, AObjT, CompareT],
      bEv: QueryElement[B, BObjT, CompareT],
  ): QueryBoolOps[A, B] = { (a, b, o) =>
    QueryBool(
      s"${aEv.getCol(a).ref.toStringNoType} $o ${bEv.getCol(b).ref.toStringNoType}",
      true,
      false,
      QueryInputMapper.empty,
    )
  }

  implicit def queryElement_codeElement[AObjT, BObjT, CompareT, A, B](implicit
      aEv: QueryElement[A, AObjT, CompareT],
      bEv: CodeElement[B, BObjT, CompareT],
      convert: MapInput[BObjT, AObjT],
  ): QueryBoolOps[A, B] = { (a, b, o) =>
    QueryBool(
      s"${aEv.getCol(a).ref.toStringNoType} $o ${aEv.getCol(a).col.`(?)`}",
      true,
      false,
      QueryInputMapper(_ => 1, (in, out, off) => out(off) = aEv.getCol(a).col.colCodec.encoder.encodeColumn(convert.f(bEv.getObj(b, in)))),
    )
  }

  implicit def codeElement_queryElement[AObjT, BObjT, CompareT, A, B](implicit
      aEv: CodeElement[A, AObjT, CompareT],
      bEv: QueryElement[B, BObjT, CompareT],
      convert: MapInput[AObjT, BObjT],
  ): QueryBoolOps[A, B] = { (a, b, o) =>
    QueryBool(
      s"${bEv.getCol(b).ref.toStringNoType} $o ${bEv.getCol(b).col.`(?)`}",
      true,
      false,
      QueryInputMapper(_ => 1, (in, out, off) => out(off) = bEv.getCol(b).col.colCodec.encoder.encodeColumn(convert.f(aEv.getObj(a, in)))),
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
