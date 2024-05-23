package harness.sql.query

import cats.syntax.option.*
import harness.sql.*
import harness.sql.typeclass.*
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

  final case class CodeElement[EvT, ObjT, CompareT] private (encode: (EvT, QueryEncoderMany[ObjT]) => QueryInputMapper)
  object CodeElement {

    implicit def queryInput[T](implicit notOpt: NotGiven[OptionEv[T]]): CodeElement[QueryInputVar[T], T, T] =
      CodeElement { (input, encoder) => QueryInputMapper.single(_(input.idx).asInstanceOf[T], encoder) }
    implicit def queryInputOpt[T](implicit notOpt: NotGiven[OptionEv[T]]): CodeElement[QueryInputVar[Option[T]], Option[T], T] =
      CodeElement { (input, encoder) => QueryInputMapper.single(_(input.idx).asInstanceOf[Option[T]], encoder) }

    implicit def const[T](implicit notOpt: NotGiven[OptionEv[T]]): CodeElement[Constant[T], T, T] =
      CodeElement { (const, encoder) => QueryInputMapper.materialize(const, encoder) }
    implicit def constOpt[T](implicit notOpt: NotGiven[OptionEv[T]]): CodeElement[Constant[Option[T]], Option[T], T] =
      CodeElement { (const, encoder) => QueryInputMapper.materialize(const, encoder) }

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
      fr"${aEv.getCol(a)} $o ${bEv.getCol(b)}",
      true,
      false,
    )
  }

  implicit def queryElement_codeElement[AObjT, BObjT, CompareT, A, B](implicit
      aEv: QueryElement[A, AObjT, CompareT],
      bEv: CodeElement[B, BObjT, CompareT],
      convert: MapInput[BObjT, AObjT],
  ): QueryBoolOps[A, B] = { (a, b, o) =>
    val aCol = aEv.getCol(a)
    val bFragment = Fragment(
      "?",
      bEv.encode(b, aCol.col.codec.encoder.cmap(convert.f)),
    )

    QueryBool(
      fr"$aCol $o $bFragment",
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
