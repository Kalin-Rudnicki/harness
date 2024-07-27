package harness.core

import cats.syntax.option.*
import scala.annotation.tailrec
import scala.collection.mutable

sealed trait IndentedString {

  // TODO (KR) : Possibly optimize this
  def toString(idtStr: String): String = {
    val stringBuilder = new mutable.StringBuilder
    var first = true

    def rec(
        indentedString: IndentedString,
        idt: Int,
    ): Unit = {
      def appendIndent(): Unit = {
        if (first)
          first = false
        else
          stringBuilder.append('\n')
        0.until(idt).foreach(_ => stringBuilder.append(idtStr))
      }

      indentedString match {
        case IndentedString.Break =>
          appendIndent()
        case IndentedString.Str(str) =>
          appendIndent()
          stringBuilder.append(str)
        case IndentedString.StrWithJoin(str, _) =>
          appendIndent()
          stringBuilder.append(str)
        case IndentedString.Inline(children) =>
          children.foreach(rec(_, idt))
        case IndentedString.Indented(children) =>
          children.foreach(rec(_, idt + 1))
      }
    }

    rec(IndentedString.collapseStrings(this), 0)
    stringBuilder.toString
  }

  def nonInlines: List[IndentedString.NonInline] = this match
    case self: IndentedString.NonInline  => self :: Nil
    case IndentedString.Inline(children) => children

  def withoutInlineWrapper: IndentedString = this match
    case IndentedString.Inline(self :: Nil) => self
    case _                                  => this

  override def toString: String = toString("    ")

}

object IndentedString {

  sealed trait NonInline extends IndentedString

  // =====| ADT |=====

  case object Break extends IndentedString.NonInline

  final case class Str(str: String) extends IndentedString.NonInline

  // TODO (KR) : final case class JoinString(str: String) extends IndentedString.NonInline
  final case class StrWithJoin(str: String, joinUsing: String) extends IndentedString.NonInline

  final case class Inline(children: List[IndentedString.NonInline]) extends IndentedString

  final case class Indented(children: List[IndentedString.NonInline]) extends IndentedString.NonInline

  // Public Helpers

  def inline(children: IndentedString*): IndentedString =
    Inline(children.toList.flatMap(_.nonInlines))

  def indented(children: IndentedString*): IndentedString =
    Indented(children.toList.flatMap(_.nonInlines))

  def section(header: String)(body: IndentedString*): IndentedString =
    IndentedString.inline(header, IndentedString.indented(body*))
  
  def fromAny(any: Any)(show: PartialFunction[Matchable, IndentedString]): IndentedString = fromAny(any, show.lift)
  def fromAny(any: Any): IndentedString = fromAny(any, _ => None)

  // Conversions

  trait ToIndentedString[-T] {
    def convert(t: T): IndentedString
  }

  implicit def convert[T: ToIndentedString](t: T): IndentedString =
    implicitly[ToIndentedString[T]].convert(t)

  implicit val indentedStringToIndentedString: ToIndentedString[IndentedString] = identity(_)

  implicit val stringToIndentedString: ToIndentedString[String] = Str(_)

  implicit def optionToIndentedString[T: ToIndentedString]: ToIndentedString[Option[? <: T]] = { opt =>
    val toIdtStr = implicitly[ToIndentedString[T]]
    Inline(opt.map(toIdtStr.convert).toList.flatMap(_.nonInlines)).withoutInlineWrapper
  }

  implicit def listToIndentedString[T: ToIndentedString]: ToIndentedString[List[? <: T]] = { list =>
    val toIdtStr = implicitly[ToIndentedString[T]]
    Inline(list.map(toIdtStr.convert).flatMap(_.nonInlines)).withoutInlineWrapper
  }

  implicit def seqToIndentedString[T: ToIndentedString]: ToIndentedString[Seq[? <: T]] = { seq =>
    val toIdtStr = implicitly[ToIndentedString[T]]
    Inline(seq.toList.map(toIdtStr.convert).flatMap(_.nonInlines)).withoutInlineWrapper
  }

  // Private Helpers

  private def fromAnyProduct(product: Product, show: Matchable => Option[IndentedString]): IndentedString =
    IndentedString.inline(
      s"[${product.productPrefix}]:",
      IndentedString.indented(
        product.productElementNames
          .zip(product.productIterator)
          .toList
          .flatMap { case (key, value) =>
            List(
              IndentedString.StrWithJoin(s"$key:", " "),
              fromAny(value, show),
            )
          } *,
      ),
    )

  private def fromAny(any: Any, show: Matchable => Option[IndentedString]): IndentedString = {
    val matchable: Matchable = any.asInstanceOf[Matchable]
    show(matchable) match {
      case Some(is) => is
      case None =>
        matchable match {
          case option: Option[?] => fromAnyProduct(option, show)
          case seq: List[?]      => IndentedString.inline("[List[_]]:", IndentedString.indented(seq.map(fromAny(_, show))*))
          case seq: Vector[?]    => IndentedString.inline("[Vector[_]]:", IndentedString.indented(seq.map(fromAny(_, show))*))
          case seq: Seq[?]       => IndentedString.inline("[Seq[_]]:", IndentedString.indented(seq.map(fromAny(_, show))*))
          case seq: Array[?]     => IndentedString.inline("[Array[_]]:", IndentedString.indented(seq.map(fromAny(_, show))*))
          case product: Product  => fromAnyProduct(product, show)
          case _                 => IndentedString.Str(any.toString)
        }
    }
  }

  private def collapseStringsList(iss: List[IndentedString.NonInline]): List[IndentedString.NonInline] = {
    def prependHeldString(heldString: Option[IndentedString.StrWithJoin], rFinalized: List[IndentedString.NonInline]): List[IndentedString.NonInline] =
      heldString match {
        case None        => rFinalized
        case Some(value) => IndentedString.Str(value.str) :: rFinalized
      }

    @tailrec
    def loop(
        queue: List[IndentedString.NonInline],
        heldString: Option[IndentedString.StrWithJoin],
        rFinalized: List[IndentedString.NonInline],
    ): List[IndentedString.NonInline] =
      queue match {
        case head :: tail =>
          head match {
            case IndentedString.Break =>
              loop(tail, None, IndentedString.Break :: prependHeldString(heldString, rFinalized))
            case IndentedString.Indented(children) =>
              loop(tail, None, IndentedString.Indented(collapseStringsList(children)) :: prependHeldString(heldString, rFinalized))
            case is: IndentedString.Str =>
              heldString match {
                case Some(heldString) => loop(tail, None, IndentedString.Str(s"${heldString.str}${heldString.joinUsing}${is.str}") :: rFinalized)
                case None             => loop(tail, None, is :: rFinalized)
              }
            case is: IndentedString.StrWithJoin =>
              heldString match {
                case Some(heldString) => loop(tail, StrWithJoin(s"${heldString.str}${heldString.joinUsing}${is.str}", is.joinUsing).some, rFinalized)
                case None             => loop(tail, is.some, rFinalized)
              }
          }
        case Nil =>
          prependHeldString(heldString, rFinalized).reverse
      }

    loop(iss, None, Nil)
  }

  private def collapseStrings(is: IndentedString): IndentedString = is match
    case IndentedString.Inline(children)   => IndentedString.Inline(collapseStringsList(children))
    case IndentedString.Indented(children) => IndentedString.Indented(collapseStringsList(children))
    case _                                 => is

}
