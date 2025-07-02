package harness.deriving

import scala.quoted.*

trait IntersectionMirror[A] {
  type ElementTypes <: Tuple
}
object IntersectionMirror {

  final class Impl[A, T <: Tuple] extends IntersectionMirror[A] {
    override type ElementTypes = T
  }

  transparent inline given derived: [A] => IntersectionMirror[A] = ${ derivedImpl[A] }

  private def derivedImpl[A](using quotes: Quotes, tpe: Type[A]): Expr[IntersectionMirror[A]] = {
    import quotes.reflect.*

    type Elems <: Tuple

    def expandTypes(repr: TypeRepr): List[TypeRepr] =
      repr.dealias match {
        case AndType(left, right) => expandTypes(left) ::: expandTypes(right)
        case _                    => repr :: Nil
      }

    val tupleAppend = TypeRepr.of[? *: ?].asInstanceOf[AppliedType].tycon

    val expanded: List[TypeRepr] = expandTypes(TypeRepr.of[A])

    if (expanded.size < 2)
      report.errorAndAbort(s"Type ${TypeRepr.of[A].show} is not an intersection type")

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
              TypeTree.of[IntersectionMirror.Impl],
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
    ).asExprOf[IntersectionMirror[A]]
  }

}
