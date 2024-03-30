package harness.deriving

import harness.deriving.internal.AnnotationMacros
import scala.quoted.*

trait FieldAnnotations[Annotated, Annotation] {
  type Out <: Tuple
  def annotations: Out
  final def annotationsList: List[Option[Annotation]] =
    annotations.toIArray.toList.asInstanceOf[List[Option[Annotation]]]
}
object FieldAnnotations {

  def apply[Annotated, Annotation](implicit annotations: FieldAnnotations[Annotated, Annotation]): Aux[Annotated, Annotation, annotations.Out] = annotations

  type Aux[Annotated, Annotation, _Out <: Tuple] = FieldAnnotations[Annotated, Annotation] { type Out = _Out }

  def make[Annotated, Annotation, _Out <: Tuple](_annotations: _Out): Aux[Annotated, Annotation, _Out] =
    new FieldAnnotations[Annotated, Annotation] {
      override type Out = _Out
      override def annotations: Out = _annotations
    }

  inline def mkAnnotations[Annotated, Annotation] = ${ AnnotationMacros.mkFieldAnnotations[Annotated, Annotation] }

  inline given [Annotated, Annotation]: FieldAnnotations[Annotated, Annotation] = mkAnnotations[Annotated, Annotation]

}
