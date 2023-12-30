package harness.sql.typeclass

import cats.data.EitherNel
import cats.syntax.either.*
import cats.syntax.option.*
import harness.core.Zip
import harness.sql.*
import java.sql.ResultSet
import scala.annotation.tailrec
import shapeless3.deriving.*

trait RowDecoder[T] { self =>

  lazy val width: Int
  lazy val classes: IArray[Option[Class[_]]]
  def decodeRow(o: Int, arr: IArray[Object]): EitherNel[String, T]

  final def map[T2](f: T => T2): RowDecoder[T2] =
    new RowDecoder[T2] {
      override lazy val width: Int = self.width
      override lazy val classes: IArray[Option[Class[_]]] = self.classes
      override def decodeRow(o: Int, arr: IArray[Object]): EitherNel[String, T2] = self.decodeRow(o, arr).map(f)
    }
  final def emap[T2](f: T => EitherNel[String, T2]): RowDecoder[T2] =
    new RowDecoder[T2] {
      override lazy val width: Int = self.width
      override lazy val classes: IArray[Option[Class[_]]] = self.classes
      override def decodeRow(o: Int, arr: IArray[Object]): EitherNel[String, T2] = self.decodeRow(o, arr).flatMap(f)
    }

  final def ~[T2](other: RowDecoder[T2])(implicit z: Zip[T, T2]): RowDecoder[z.Out] =
    new RowDecoder[z.Out] {
      override lazy val width: Int = self.width + other.width
      override lazy val classes: IArray[Option[Class[_]]] = self.classes ++ other.classes
      override def decodeRow(o: Int, arr: IArray[Object]): EitherNel[String, z.Out] =
        for {
          a <- self.decodeRow(o, arr)
          b <- other.decodeRow(o + self.width, arr)
        } yield z.zip(a, b)
    }

  final def optional: RowDecoder[Option[T]] =
    new RowDecoder[Option[T]] {
      override lazy val width: Int = self.width
      override lazy val classes: IArray[Option[Class[_]]] = self.classes
      override def decodeRow(o: Int, arr: IArray[Object]): EitherNel[String, Option[T]] =
        if (o.until(o + width).forall(arr(_) == null)) None.asRight
        else self.decodeRow(o, arr).map(_.some)
    }

}
object RowDecoder {

  def fromColDecoder[T](cd: ColDecoder[T]): RowDecoder[T] =
    new RowDecoder[T] {
      override lazy val width: Int = 1
      override def decodeRow(o: Int, arr: IArray[Object]): EitherNel[String, T] = cd.decodeColumn(arr(o))
      override lazy val classes: IArray[Option[Class[_]]] = IArray(cd.klass)
    }

  def fromCol[T](c: Col[T]): RowDecoder[T] =
    new RowDecoder[T] {
      private val cd: ColDecoder[T] = c.colCodec.decoder
      override lazy val width: Int = 1
      override def decodeRow(o: Int, arr: IArray[Object]): EitherNel[String, T] = cd.decodeColumn(arr(o)).leftMap(_.map(err => s"Error decoding column '${c.colName}': $err"))
      override lazy val classes: IArray[Option[Class[_]]] = IArray(cd.klass)
    }

  def forTable[T[_[_]] <: Table](t: T[Col])(using inst: => K11.ProductGeneric[T]): RowDecoder[T[Id]] =
    new RowDecoder[T[Id]] {
      private lazy val cols: IArray[Col[Any]] = inst.toRepr(t).toIArray.map(_.asInstanceOf[Col[Any]])
      private lazy val colNames: IArray[String] = cols.map(_.colName)
      private lazy val decoders: IArray[ColDecoder[Any]] = cols.map(_.colCodec.decoder)

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
                errors.map(err => s"Error decoding column '${colNames(idx)}': $err").asLeft
            }
          else
            inst.fromRepr(Tuple.fromArray(parsed).asInstanceOf).asInstanceOf[T[Id]].asRight

        loop(0)
      }
      override lazy val classes: IArray[Option[Class[_]]] = decoders.map(_.klass)
    }

}
