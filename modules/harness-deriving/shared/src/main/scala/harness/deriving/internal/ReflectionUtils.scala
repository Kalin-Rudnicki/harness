package harness.deriving.internal

import scala.annotation.tailrec
import scala.deriving.*
import scala.quoted.*

@scala.annotation.nowarn
final class ReflectionUtils[Q <: Quotes & Singleton](val q: Q) {
  given q.type = q
  import q.reflect.*

  private implicit class OptionOps[A](self: Option[A]) {
    def collectM[B](f: PartialFunction[A & Matchable, B]): Option[B] =
      self.asInstanceOf[Option[A & Matchable]].collect(f)
  }
  private implicit class ListOps[A](self: List[A]) {
    def collectM[B](f: PartialFunction[A & Matchable, B]): List[B] =
      self.asInstanceOf[List[A & Matchable]].collect(f)
  }

  // =====|  |=====

  final case class Mirror(
      MirroredType: TypeRepr,
      MirroredMonoType: TypeRepr,
      MirroredElemTypes: Seq[TypeRepr],
      MirroredLabel: String,
      MirroredElemLabels: Seq[String],
  )
  object Mirror {

    def apply(mirror: Expr[scala.deriving.Mirror]): Option[Mirror] = for
      mirrorTpe <- Some(mirror.asTerm.tpe.widen)
      mt <- findMemberType(mirrorTpe, "MirroredType")
      mmt <- findMemberType(mirrorTpe, "MirroredMonoType")
      mets <- findMemberType(mirrorTpe, "MirroredElemTypes")
      ml <- findMemberType(mirrorTpe, "MirroredLabel").collectM { case ConstantType(StringConstant(ml)) => ml }
      mels <- findMemberType(mirrorTpe, "MirroredElemLabels")
      labels = tupleTypeElements(mels).collectM { case ConstantType(StringConstant(l)) => l }
    yield Mirror(mt, mmt, tupleTypeElements(mets), ml, labels)

    def apply(tpe: TypeRepr): Option[Mirror] =
      val MirrorType = TypeRepr.of[scala.deriving.Mirror]
      val mtpe = Refinement(MirrorType, "MirroredType", TypeBounds(tpe, tpe))
      val instance = Implicits.search(mtpe) match {
        case iss: ImplicitSearchSuccess => Some(iss.tree.asExprOf[scala.deriving.Mirror])
        case _: ImplicitSearchFailure   => None
      }
      instance.flatMap(Mirror(_))

  }

  private def tupleTypeElements(tp: TypeRepr): List[TypeRepr] = {
    @tailrec
    def loop(tp: TypeRepr, acc: List[TypeRepr]): List[TypeRepr] =
      tp match {
        case AppliedType(_, List(hd: TypeRepr, tl: TypeRepr)) => loop(tl, hd :: acc)
        case _                                                => acc
      }

    loop(tp, Nil).reverse
  }

  private def low(tp: TypeRepr): TypeRepr =
    tp match {
      case tp: TypeBounds => tp.low
      case tp             => tp
    }

  private def findMemberType(tp: TypeRepr, name: String): Option[TypeRepr] =
    tp match {
      case Refinement(_, `name`, tp) => Some(low(tp))
      case Refinement(parent, _, _)  => findMemberType(parent, name)
      case AndType(left, right)      => findMemberType(left, name).orElse(findMemberType(right, name))
      case _                         => None
    }

}
