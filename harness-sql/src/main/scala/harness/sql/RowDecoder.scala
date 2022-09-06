package harness.sql

import cats.data.EitherNel
import cats.syntax.either.*

trait RowDecoder[T] { self =>

  def decodeRow(s: List[ColT]): EitherNel[String, (T, List[ColT])]

  final def map[T2](f: T => T2): RowDecoder[T2] = self.decodeRow(_).map { case (t, cs) => (f(t), cs) }
  final def emap[T2](f: T => EitherNel[String, T2]): RowDecoder[T2] = self.decodeRow(_).flatMap { case (t, cs) => f(t).map((_, cs)) }

}
object RowDecoder {

  def fromColDecoder[T](cd: ColDecoder[T]): RowDecoder[T] = {
    case head :: tail => cd.decodeColumn(head).map((_, tail))
    case Nil          => "Not enough columns to decode".leftNel
  }

}
