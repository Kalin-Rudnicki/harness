package harness.xml

import cats.data.{EitherNel, NonEmptyList}
import cats.syntax.either.*
import cats.syntax.option.*
import cats.syntax.parallel.*
import harness.core.{StringDecoder, Zip}
import scala.xml.{Node, NodeSeq, Text}

trait XmlDecoder[T] { self =>

  // TODO (KR) : improve to show nesting
  def decodeAccumulating(nodeSeq: Seq[Node]): EitherNel[String, T]

  final def decode(nodeSeq: Seq[Node]): Either[String, T] = self.decodeAccumulating(nodeSeq).leftMap(_.head)

  def map[T2](f: T => T2): XmlDecoder[T2] = self.decodeAccumulating(_).map(f)
  def emap[T2](f: T => EitherNel[String, T2]): XmlDecoder[T2] = self.decodeAccumulating(_).flatMap(f)

  final def <*>[T2](other: XmlDecoder[T2])(implicit zip: Zip[T, T2]): XmlDecoder[zip.Out] =
    nodeSeq => (self.decodeAccumulating(nodeSeq), other.decodeAccumulating(nodeSeq)).parMapN(zip.zip)

  final def /:(nodeName: String): XmlDecoder.SingleNodeDecoder[T] = XmlDecoder.SingleNodeDecoder(nodeName, self)
  final def inNode(nodeName: String): XmlDecoder.SingleNodeDecoder[T] = XmlDecoder.SingleNodeDecoder(nodeName, self)

  final def someOrElse[T2](default: => T2)(implicit ev: T <:< Option[T2]): XmlDecoder[T2] =
    self.decodeAccumulating(_).map(_.getOrElse(default))

}
object XmlDecoder {

  def text[T](f: String => EitherNel[String, T]): XmlDecoder.TextDecoder[T] = XmlDecoder.TextDecoder(f)
  def textFromStringDecoder[T](implicit decoder: StringDecoder[T]): XmlDecoder.TextDecoder[T] = XmlDecoder.TextDecoder(decoder.decodeAccumulating)

  def node[T](nodeName: String)(innerDecoder: XmlDecoder[T]): XmlDecoder.SingleNodeDecoder[T] = XmlDecoder.SingleNodeDecoder(nodeName, innerDecoder)

  def pure[T](value: T): XmlDecoder[T] =
    _ => value.asRight

  // =====|  |=====

  private def decodeNodes[T](nodeName: String, nodeSeq: Seq[Node])(f: List[Seq[Node]] => EitherNel[String, T]): EitherNel[String, T] =
    f(nodeSeq.collect { case node if node.label == nodeName => node.child.toSeq }.toList)

  final class SingleNodeDecoder[T](
      nodeName: String,
      innerDecoder: XmlDecoder[T],
  ) extends XmlDecoder[T] { self =>

    override def decodeAccumulating(nodeSeq: Seq[Node]): EitherNel[String, T] =
      XmlDecoder.decodeNodes(nodeName, nodeSeq) {
        case childNodes :: Nil => innerDecoder.decodeAccumulating(childNodes)
        case list              => s"Expected 1 '$nodeName' node, but got ${list.length}".leftNel
      }

    override def map[T2](f: T => T2): XmlDecoder.SingleNodeDecoder[T2] = XmlDecoder.SingleNodeDecoder(nodeName, innerDecoder.map(f))
    override def emap[T2](f: T => EitherNel[String, T2]): XmlDecoder.SingleNodeDecoder[T2] = XmlDecoder.SingleNodeDecoder(nodeName, innerDecoder.emap(f))

    def optional: XmlDecoder.OptionalNodeDecoder[T] = XmlDecoder.OptionalNodeDecoder(nodeName, innerDecoder)
    def list: XmlDecoder.ListNodeDecoder[T] = XmlDecoder.ListNodeDecoder(nodeName, innerDecoder)
    def nonEmptyList: XmlDecoder.NonEmptyListNodeDecoder[T] = XmlDecoder.NonEmptyListNodeDecoder(nodeName, innerDecoder)

  }

  final class OptionalNodeDecoder[T](
      nodeName: String,
      innerDecoder: XmlDecoder[T],
  ) extends XmlDecoder[Option[T]] {

    override def decodeAccumulating(nodeSeq: Seq[Node]): EitherNel[String, Option[T]] =
      XmlDecoder.decodeNodes(nodeName, nodeSeq) {
        case childNodes :: Nil => innerDecoder.decodeAccumulating(childNodes).map(_.some)
        case Nil               => None.asRight
        case list              => s"Expected 0..1 '$nodeName' node, but got ${list.length}".leftNel
      }

  }

  final class ListNodeDecoder[T](
      nodeName: String,
      innerDecoder: XmlDecoder[T],
  ) extends XmlDecoder[List[T]] {

    override def decodeAccumulating(nodeSeq: Seq[Node]): EitherNel[String, List[T]] =
      XmlDecoder.decodeNodes(nodeName, nodeSeq) { _.parTraverse(innerDecoder.decodeAccumulating) }

  }

  final class NonEmptyListNodeDecoder[T](
      nodeName: String,
      innerDecoder: XmlDecoder[T],
  ) extends XmlDecoder[NonEmptyList[T]] {

    override def decodeAccumulating(nodeSeq: Seq[Node]): EitherNel[String, NonEmptyList[T]] =
      XmlDecoder.decodeNodes(nodeName, nodeSeq) {
        case head :: tail => NonEmptyList(head, tail).parTraverse(innerDecoder.decodeAccumulating)
        case Nil          => s"Expected 1+ '$nodeName' node, but got 0".leftNel
      }

  }

  final class TextDecoder[T](
      _decode: String => EitherNel[String, T],
  ) extends XmlDecoder[T] {

    override def decodeAccumulating(nodeSeq: Seq[Node]): EitherNel[String, T] =
      nodeSeq.toList match {
        case (text: Text) :: Nil => _decode(text.data)
        case _                   => s"Expected single Text node, but got: ${nodeSeq.map(_.getClass.getSimpleName).mkString("[", ", ", "]")}".leftNel
      }

  }

}
