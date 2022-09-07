package harness.sql.typeclass

import cats.data.EitherNel
import cats.syntax.either.*
import harness.core.Zip
import harness.sql.*
import scala.annotation.tailrec
import shapeless3.deriving.*

trait RowDecoder[T] { self =>

  def decodeRow(s: List[ColT]): EitherNel[String, (T, List[ColT])]

  final def map[T2](f: T => T2): RowDecoder[T2] = self.decodeRow(_).map { case (t, cs) => (f(t), cs) }
  final def emap[T2](f: T => EitherNel[String, T2]): RowDecoder[T2] = self.decodeRow(_).flatMap { case (t, cs) => f(t).map((_, cs)) }

  final def ~[T2](other: RowDecoder[T2])(implicit zip: Zip[T, T2]): RowDecoder[zip.Out] = { cols =>
    for {
      (t1, cols) <- self.decodeRow(cols)
      (t2, cols) <- other.decodeRow(cols)
    } yield (zip.zip(t1, t2), cols)
  }

}
object RowDecoder {

  def fromColDecoder[T](cd: ColDecoder[T]): RowDecoder[T] = {
    case head :: tail => cd.decodeColumn(head).map((_, tail))
    case Nil          => "Not enough columns to decode".leftNel
  }

  def forTable[T[_[_]] <: Table](t: T[RowDecoder])(using inst: => K11.ProductGeneric[T]): RowDecoder[T[cats.Id]] = { cols =>
    val decoders: List[RowDecoder[Any]] = inst.toRepr(t).toList.asInstanceOf[List[RowDecoder[Any]]]

    @tailrec
    def loop(rAccum: List[Any], decoders: List[RowDecoder[Any]], cols: List[ColT]): EitherNel[String, (T[cats.Id], List[ColT])] =
      decoders match {
        case head :: tail =>
          head.decodeRow(cols) match {
            case Right((value, cols)) => loop(value :: rAccum, tail, cols)
            case Left(errs)           => errs.asLeft
          }
        case Nil => (inst.fromRepr(Tuple.fromArray(rAccum.reverse.toArray).asInstanceOf), cols).asRight
      }

    loop(Nil, decoders, cols)
  }

}
