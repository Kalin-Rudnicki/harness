package harness.sql.typeclass

import cats.data.EitherNel
import cats.syntax.either.*
import harness.core.Zip
import harness.sql.*
import scala.annotation.tailrec
import shapeless3.deriving.*

trait RowDecoder[T] { self =>

  lazy val width: Int
  def decodeRow(o: Int, arr: IArray[Object]): EitherNel[String, T]

  final def map[T2](f: T => T2): RowDecoder[T2] =
    new RowDecoder[T2] {
      override lazy val width: Int = self.width
      override def decodeRow(o: Int, arr: IArray[Object]): EitherNel[String, T2] = self.decodeRow(o, arr).map(f)
    }
  final def emap[T2](f: T => EitherNel[String, T2]): RowDecoder[T2] =
    new RowDecoder[T2] {
      override lazy val width: Int = self.width
      override def decodeRow(o: Int, arr: IArray[Object]): EitherNel[String, T2] = self.decodeRow(o, arr).flatMap(f)
    }

  final def ~[T2](other: RowDecoder[T2])(implicit zip: Zip[T, T2]): RowDecoder[zip.Out] =
    new RowDecoder[zip.Out] {
      override lazy val width: Int = self.width + other.width
      override def decodeRow(o: Int, arr: IArray[Object]): EitherNel[String, zip.Out] =
        for {
          a <- self.decodeRow(o, arr)
          b <- other.decodeRow(o + self.width, arr)
        } yield zip.zip(a, b)
    }

}
object RowDecoder {

  def fromColDecoder[T](cd: ColDecoder[T]): RowDecoder[T] =
    new RowDecoder[T] {
      override lazy val width: Int = 1
      override def decodeRow(o: Int, arr: IArray[Object]): EitherNel[String, T] = cd.decodeColumn(arr(o))
    }

  def forTable[T[_[_]] <: Table](t: T[ColDecoder])(using inst: => K11.ProductGeneric[T]): RowDecoder[T[Id]] =
    new RowDecoder[T[Id]] {
      lazy val decoders: IArray[ColDecoder[Any]] = inst.toRepr(t).toIArray.asInstanceOf[IArray[ColDecoder[Any]]]
      override lazy val width: Int = decoders.length
      override def decodeRow(o: Int, arr: IArray[Object]): EitherNel[String, T[Id]] = {
        val parsed: Array[Any] = new Array[Any](width)

        @tailrec
        def loop(idx: Int): EitherNel[String, T[Id]] =
          if (idx < width)
            decoders(idx).decodeColumn(arr(o + idx)) match {
              case Right(value) =>
                parsed(idx) = value
                loop(idx + 1)
              case Left(errors) =>
                errors.asLeft
            }
          else
            inst.fromRepr(Tuple.fromArray(parsed).asInstanceOf).asInstanceOf[T[Id]].asRight

        loop(0)
      }
    }

}
