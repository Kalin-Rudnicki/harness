package harness.deriving

import cats.syntax.option.*
import harness.deriving.internal.AnnotationMacros
import scala.quoted.*

trait OptionalAnnotation[Annotated, Annotation] {
  def annotation: Option[Annotation]
}
object OptionalAnnotation {

  def apply[Annotated, Annotation](implicit annotation: OptionalAnnotation[Annotated, Annotation]): OptionalAnnotation[Annotated, Annotation] = annotation

  def some[Annotated, Annotation](_annotation: Annotation): OptionalAnnotation[Annotated, Annotation] =
    new OptionalAnnotation[Annotated, Annotation] {
      override def annotation: Option[Annotation] = _annotation.some
    }
  def none[Annotated, Annotation]: OptionalAnnotation[Annotated, Annotation] =
    new OptionalAnnotation[Annotated, Annotation] {
      override def annotation: Option[Annotation] = None
    }

  inline def mkAnnotation[Annotated, Annotation] = ${ AnnotationMacros.mkOptionalAnnotation[Annotated, Annotation] }

  inline given [Annotated, Annotation] => OptionalAnnotation[Annotated, Annotation] = mkAnnotation[Annotated, Annotation]

}
