package harness.sql.query

import cats.syntax.option.*
import harness.sql.*
import scala.util.NotGiven

final case class QueryBool private[sql] (
    private[sql] val fragment: Fragment,
    private[sql] val unaryNeedsParens: Boolean,
    private[sql] val binaryNeedsParens: Boolean,
) {

  def unaryFragment: Fragment = fragment.wrapInParensIf(unaryNeedsParens)
  def binaryFragment: Fragment = fragment.wrapInParensIf(binaryNeedsParens)

  def unary_! : QueryBool =
    QueryBool("NOT " +: unaryFragment, true, false)
  def &&(that: QueryBool): QueryBool =
    QueryBool(
      fr"${this.binaryFragment} AND ${that.binaryFragment}",
      true,
      true,
    )
  def ||(that: QueryBool): QueryBool =
    QueryBool(
      fr"${this.binaryFragment} OR ${that.binaryFragment}",
      true,
      true,
    )

}

given Conversion[AppliedCol[Boolean], QueryBool] = a => QueryBool(fr"$a", false, false)
given Conversion[AppliedCol.Opt[Boolean], QueryBool] = a => QueryBool(fr"$a", false, false)

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
      fr"${aEv.getCol(a).ref} $o ${bEv.getCol(b).ref}",
      true,
      false,
    )
  }

  implicit def queryElement_codeElement[AObjT, BObjT, CompareT, A, B](implicit
      aEv: QueryElement[A, AObjT, CompareT],
      bEv: CodeElement[B, BObjT, CompareT],
      convert: MapInput[BObjT, AObjT],
  ): QueryBoolOps[A, B] = { (a, b, o) =>
    QueryBool(
      fr"${aEv.getCol(a).ref} $o ${aEv.getCol(a).col.`(?)`}" ##
        QueryInputMapper.single(in => aEv.getCol(a).col.colCodec.encoder.encodeColumn(convert.f(bEv.getObj(b, in)))),
      true,
      false,
    )
  }

  implicit def codeElement_queryElement[AObjT, BObjT, CompareT, A, B](implicit
      aEv: CodeElement[A, AObjT, CompareT],
      bEv: QueryElement[B, BObjT, CompareT],
      convert: MapInput[AObjT, BObjT],
  ): QueryBoolOps[A, B] = { (a, b, o) =>
    QueryBool(
      fr"${bEv.getCol(b).ref.toStringNoType} $o ${bEv.getCol(b).col.`(?)`}" ##
        QueryInputMapper.single(in => bEv.getCol(b).col.colCodec.encoder.encodeColumn(convert.f(aEv.getObj(a, in)))),
      true,
      false,
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
  def isNull: QueryBool = QueryBool(fr"$a IS NULL", true, false)
  def isNotNull: QueryBool = QueryBool(fr"$a IS NOT NULL", true, false)
}

extension [A](a: AppliedCol.Opt[A]) {
  def isNull: QueryBool = QueryBool(fr"$a IS NULL", true, false)
  def isNotNull: QueryBool = QueryBool(fr"$a IS NOT NULL", true, false)
}

// TODO (KR) : LIKE
// TODO (KR) : IN
