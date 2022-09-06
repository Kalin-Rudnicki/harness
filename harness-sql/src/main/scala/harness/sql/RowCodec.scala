package harness.sql

import cats.data.EitherNel
import cats.syntax.either.*
import shapeless3.deriving.*

trait RowCodec[T] { self =>

  def encodeResult(t: T): List[ColT]
  def decodeResult(s: List[ColT]): EitherNel[String, (T, List[ColT])]

  final def imap[T2](mf: T => T2)(cmf: T2 => T): RowCodec[T2] =
    new RowCodec[T2] {
      override def encodeResult(t: T2): List[ColT] = self.encodeResult(cmf(t))
      override def decodeResult(s: List[ColT]): EitherNel[String, (T2, List[ColT])] = self.decodeResult(s).map { (r, s2) => (mf(r), s2) }
    }

  final def optional: RowCodec[Option[T]] =
    new RowCodec[Option[T]] {
      override def encodeResult(t: Option[T]): List[ColT] = ??? // TODO (KR) :
      override def decodeResult(s: List[ColT]): EitherNel[String, (Option[T], List[ColT])] = ??? // TODO (KR) :
    }

}
object RowCodec {

  given [T](using colCodec: ColCodec[T]): RowCodec[T] =
    new RowCodec[T] {
      override final def encodeResult(t: T): List[ColT] = colCodec.encodeColumn(t) :: Nil
      override final def decodeResult(s: List[ColT]): EitherNel[String, (T, List[ColT])] =
        s match {
          case head :: tail => colCodec.decodeColumn(head).map((_, tail))
          case Nil          => "Not enough columns to decode".leftNel
        }
    }

  given [T[_[_]] <: Table](using
      inst: => K0.ProductInstances[RowCodec, T[RowCodec]],
  ): RowCodec[T[cats.Id]] = {

    // fold right on inst
    // do the _ :* thing to make a tuple
    // tuple.asInstanceOf[inst.MirroredElemTypes]
    // inst.fromRepr(...)

    ??? // TODO (KR) :
  }

}
