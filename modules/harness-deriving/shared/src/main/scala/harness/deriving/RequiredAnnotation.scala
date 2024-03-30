package harness.deriving

import harness.deriving.internal.AnnotationMacros
import scala.quoted.*

trait RequiredAnnotation[Annotated, Annotation] {
  def annotation: Annotation
}
object RequiredAnnotation {

  def apply[Annotated, Annotation](implicit annotation: RequiredAnnotation[Annotated, Annotation]): RequiredAnnotation[Annotated, Annotation] = annotation

  def make[Annotated, Annotation](_annotation: Annotation): RequiredAnnotation[Annotated, Annotation] =
    new RequiredAnnotation[Annotated, Annotation] {
      override def annotation: Annotation = _annotation
    }

  inline def mkAnnotation[Annotated, Annotation] = ${ AnnotationMacros.mkRequiredAnnotation[Annotated, Annotation] }

  inline given [Annotated, Annotation]: RequiredAnnotation[Annotated, Annotation] = mkAnnotation[Annotated, Annotation]

}
