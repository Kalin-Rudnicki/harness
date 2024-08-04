package harness.deriving

import scala.quoted.*
import scala.quoted.Quotes

trait UnionMirror[A] {
  type ElementTypes <: Tuple
}
object UnionMirror {

  final class Impl[A, T <: Tuple] extends UnionMirror[A] {
    override type ElementTypes = T
  }

  transparent inline given derived[A]: UnionMirror[A] = ${ derivedImpl[A] }

  private def derivedImpl[A](using quotes: Quotes, tpe: Type[A]): Expr[UnionMirror[A]] = {
    import quotes.reflect.*

    type Elems <: Tuple

    @scala.annotation.nowarn
    def expandTypes(repr: TypeRepr): List[TypeRepr] =
      repr.dealias match {
        case OrType(left, right) => expandTypes(left) ::: expandTypes(right)
        case _                   => repr :: Nil
      }

    val tupleAppend = TypeRepr.of[? *: ?].asInstanceOf[AppliedType].tycon

    val expanded: List[TypeRepr] = expandTypes(TypeRepr.of[A])

    if (expanded.size < 2)
      report.errorAndAbort(s"Type ${TypeRepr.of[A].show} is not a union type")

    given Type[Elems] =
      expanded
        .foldRight(TypeRepr.of[EmptyTuple]) { case (t, acc) =>
          AppliedType(
            tupleAppend,
            List(t, acc),
          )
        }
        .asType
        .asInstanceOf[Type[Elems]]

    Apply(
      TypeApply(
        Select.unique(
          New(
            Applied(
              TypeTree.of[UnionMirror.Impl],
              List(
                TypeTree.of[A],
                TypeTree.of[Elems],
              ),
            ),
          ),
          "<init>",
        ),
        List(
          TypeTree.of[A],
          TypeTree.of[Elems],
        ),
      ),
      Nil,
    ).asExprOf[UnionMirror[A]]
  }

}
