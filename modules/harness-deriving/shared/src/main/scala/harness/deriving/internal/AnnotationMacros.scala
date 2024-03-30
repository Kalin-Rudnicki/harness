package harness.deriving.internal

import harness.deriving.*
import harness.deriving.internal.AnnotationMacroHelpers.*
import scala.quoted.*

private[deriving] object AnnotationMacros {

  def mkRequiredAnnotation[AnnotatedT: Type, AnnotationT: Type](using Quotes): Expr[RequiredAnnotation[AnnotatedT, AnnotationT]] = {
    import quotes.reflect.*

    val annotTpe = TypeRepr.of[AnnotationT]
    val annotFlags = annotTpe.typeSymbol.flags
    if (annotFlags.is(Flags.Abstract) || annotFlags.is(Flags.Trait)) report.errorAndAbort(s"Bad annotation type ${annotTpe.show} is abstract")
    else {
      val annoteeTpe = TypeRepr.of[AnnotatedT]
      val symbol = if annoteeTpe.isSingleton then annoteeTpe.termSymbol else annoteeTpe.typeSymbol
      symbol.getAnnotation(annotTpe.typeSymbol) match {
        case Some(tree) if tree.tpe <:< annotTpe => '{ RequiredAnnotation.make[AnnotatedT, AnnotationT](${ tree.asExprOf[AnnotationT] }) }
        case _                                   => report.errorAndAbort(s"No Annotation of type ${annotTpe.show} for type ${annoteeTpe.show}")
      }
    }
  }

  def mkOptionalAnnotation[AnnotatedT: Type, AnnotationT: Type](using Quotes): Expr[OptionalAnnotation[AnnotatedT, AnnotationT]] = {
    import quotes.reflect.*

    val annotTpe = TypeRepr.of[AnnotationT]
    val annotFlags = annotTpe.typeSymbol.flags
    if (annotFlags.is(Flags.Abstract) || annotFlags.is(Flags.Trait)) report.errorAndAbort(s"Bad annotation type ${annotTpe.show} is abstract")
    else {
      val annoteeTpe = TypeRepr.of[AnnotatedT]
      val symbol = if annoteeTpe.isSingleton then annoteeTpe.termSymbol else annoteeTpe.typeSymbol
      symbol.getAnnotation(annotTpe.typeSymbol) match {
        case Some(tree) if tree.tpe <:< annotTpe => '{ OptionalAnnotation.some[AnnotatedT, AnnotationT](${ tree.asExprOf[AnnotationT] }) }
        case _                                   => '{ OptionalAnnotation.none[AnnotatedT, AnnotationT] }
      }
    }
  }

  def mkFieldAnnotations[AnnotatedT: Type, AnnotationT: Type](using Quotes): Expr[FieldAnnotations[AnnotatedT, AnnotationT]] =
    mkAnnotationsImpl[AnnotatedT, AnnotationT, FieldAnnotations](ofExprVariableAnnotations)

  def mkTypeAnnotations[AnnotatedT: Type, AnnotationT: Type](using Quotes): Expr[TypeAnnotations[AnnotatedT, AnnotationT]] =
    mkAnnotationsImpl[AnnotatedT, AnnotationT, TypeAnnotations](ofExprTypeAnnotations)

}
