package harness.deriving.internal

import harness.deriving.*
import scala.annotation.tailrec
import scala.quoted.*

private[deriving] object AnnotationMacroHelpers {

  def mkAnnotationsImpl[AnnotatedT: Type, AnnotationT: Type, AS[AnnotatedT, AnnotationT]: Type](mk: Seq[Expr[Any]] => Expr[AS[AnnotatedT, AnnotationT]])(using
      Quotes,
  ): Expr[AS[AnnotatedT, AnnotationT]] = {
    import quotes.reflect.*

    val tpe = TypeRepr.of[AS[AnnotatedT, AnnotationT]] <:< TypeRepr.of[TypeAnnotations[AnnotatedT, AnnotationT]]
    val annotTpe = TypeRepr.of[AnnotationT]
    val annotFlags = annotTpe.typeSymbol.flags
    if (annotFlags.is(Flags.Abstract) || annotFlags.is(Flags.Trait)) report.errorAndAbort(s"Bad annotation type ${annotTpe.show} is abstract")
    else {
      val annotations = extractAnnotations[AnnotatedT](tpe)
      val exprs = annotations.map { child =>
        child.find(_.tpe <:< TypeRepr.of[AnnotationT]) match
          case Some(tree) => '{ Some(${ tree.asExprOf[AnnotationT] }) }
          case None       => '{ None }
      }

      mk(exprs)
    }
  }

  def ofExprVariableAnnotations[AnnotatedT: Type, AnnotationT: Type](annotTrees: Seq[Expr[Any]])(using Quotes): Expr[FieldAnnotations[AnnotatedT, AnnotationT]] =
    (Expr.ofTupleFromSeq(annotTrees): @unchecked) match {
      case '{ $t: tup } => '{ FieldAnnotations.make[AnnotatedT, AnnotationT, tup & Tuple]($t) }
    }

  def ofExprTypeAnnotations[AnnotatedT: Type, AnnotationT: Type](annotTrees: Seq[Expr[Any]])(using Quotes): Expr[TypeAnnotations[AnnotatedT, AnnotationT]] =
    (Expr.ofTupleFromSeq(annotTrees): @unchecked) match {
      case '{ $t: tup } => '{ TypeAnnotations.make[AnnotatedT, AnnotationT, tup & Tuple]($t) }
    }

  def extractAnnotations[AnnotatedT: Type](tpe: Boolean)(using q: Quotes): Seq[List[q.reflect.Term]] = {
    val utils = new ReflectionUtils(q)
    import quotes.reflect.*
    import utils.*

    @tailrec
    def typeAnnotationsOfType(tpe: TypeRepr, acc: List[Term]): List[Term] =
      tpe match {
        case annotated: AnnotatedType                       => typeAnnotationsOfType(annotated.underlying, annotated.annotation :: acc)
        case alias: TypeRef if alias.typeSymbol.isAliasType => typeAnnotationsOfType(alias.translucentSuperType, acc)
        case _                                              => acc
      }

    def typeAnnotationsOfTree(tree: Tree, acc: List[Term]): List[Term] =
      tree match {
        case classDef: ClassDef   => classDef.parents.flatMap(typeAnnotationsOfTree(_, acc))
        case valDef: ValDef       => typeAnnotationsOfTree(valDef.tpt, acc)
        case typeId: TypeIdent    => typeAnnotationsOfType(typeId.tpe, acc)
        case inferred: Inferred   => typeAnnotationsOfType(inferred.tpe, acc)
        case annotated: Annotated => typeAnnotationsOfTree(annotated.arg, annotated.annotation :: acc)
        case _                    => acc
      }

    def annotationsOfSym(sym: Symbol): List[Term] =
      if tpe then typeAnnotationsOfTree(sym.tree, Nil) else sym.annotations.reverse

    def annotationsOfType(tpe: TypeRepr): List[Term] =
      annotationsOfSym(if tpe.isSingleton then tpe.termSymbol else tpe.typeSymbol)

    val annoteeTpe = TypeRepr.of[AnnotatedT]
    annoteeTpe.classSymbol match
      case Some(annoteeCls) if annoteeCls.flags.is(Flags.Case) =>
        annoteeCls.primaryConstructor.paramSymss.find(_.exists(_.isTerm)).getOrElse(Nil).map(annotationsOfSym)
      case Some(_) =>
        Mirror(annoteeTpe) match
          case Some(mirror) => mirror.MirroredElemTypes.map(annotationsOfType)
          case None         => report.errorAndAbort(s"No Annotations for type ${annoteeTpe.show} without a Mirror")
      case None =>
        report.errorAndAbort(s"No Annotations for non-class ${annoteeTpe.show}")
  }

}
