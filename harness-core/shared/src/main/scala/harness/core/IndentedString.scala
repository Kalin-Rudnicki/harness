package harness.core

import scala.collection.mutable
import scala.language.implicitConversions

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
        case IndentedString.Inline(children) =>
          children.foreach(rec(_, idt))
        case IndentedString.Indented(children) =>
          children.foreach(rec(_, idt + 1))
      }
    }

    rec(this, 0)
    stringBuilder.toString
  }

  override def toString: String = toString("    ")

}

object IndentedString {

  // =====| ADT |=====

  case object Break extends IndentedString

  final case class Str(str: String) extends IndentedString

  final case class Inline(children: List[IndentedString]) extends IndentedString

  final case class Indented(children: List[IndentedString]) extends IndentedString

  // Helpers

  def inline(children: IndentedString*): IndentedString =
    Inline(children.toList)

  def indented(children: IndentedString*): IndentedString =
    Indented(children.toList)

  // Conversions

  trait ToIndentedString[-T] {

    def convert(t: T): IndentedString

  }

  implicit def convert[T: ToIndentedString](t: T): IndentedString =
    implicitly[ToIndentedString[T]].convert(t)

  implicit val indentedStringToIndentedString: ToIndentedString[IndentedString] = identity(_)

  implicit val stringToIndentedString: ToIndentedString[String] = Str(_)

  implicit def optionToIndentedString[T: ToIndentedString]: ToIndentedString[Option[_ <: T]] = { opt =>
    val toIdtStr = implicitly[ToIndentedString[T]]
    Inline(opt.map(toIdtStr.convert).toList)
  }

  implicit def listToIndentedString[T: ToIndentedString]: ToIndentedString[List[_ <: T]] = { list =>
    val toIdtStr = implicitly[ToIndentedString[T]]
    Inline(list.map(toIdtStr.convert))
  }

}
