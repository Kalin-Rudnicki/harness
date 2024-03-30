package harness.deriving

import harness.deriving.internal.AnnotationMacros
import scala.quoted.*

trait TypeAnnotations[Annotated, Annotation] extends Serializable {
  type Out <: Tuple
  def annotations: Out
}

object TypeAnnotations:

  def apply[Annotated, Annotation](implicit annotations: TypeAnnotations[Annotated, Annotation]): Aux[Annotated, Annotation, annotations.Out] = annotations

  type Aux[Annotated, Annotation, Out0 <: Tuple] = TypeAnnotations[Annotated, Annotation] { type Out = Out0 }

  def make[Annotated, Annotation, _Out <: Tuple](_annotations: _Out): Aux[Annotated, Annotation, _Out] =
    new TypeAnnotations[Annotated, Annotation] {
      override type Out = _Out
      override def annotations: Out = _annotations
    }

  transparent inline given mkAnnotations[Annotated, Annotation]: TypeAnnotations[Annotated, Annotation] =
    ${ AnnotationMacros.mkTypeAnnotations[Annotated, Annotation] }
