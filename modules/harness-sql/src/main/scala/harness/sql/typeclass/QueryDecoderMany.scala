package harness.sql.typeclass

import cats.data.EitherNel
import cats.syntax.either.*
import cats.syntax.option.*
import harness.core.Zip
import harness.deriving.*
import harness.deriving.K11.*
import harness.sql.*
import scala.annotation.tailrec
import zio.Chunk

trait QueryDecoderMany[T] { self =>

  lazy val width: Int
  lazy val classes: Chunk[Option[Class[?]]]
  def decodeMany(o: Int, arr: Chunk[Object]): EitherNel[String, T]

  final def map[T2](f: T => T2): QueryDecoderMany[T2] =
    new QueryDecoderMany[T2] {
      override lazy val width: Int = self.width
      override lazy val classes: Chunk[Option[Class[?]]] = self.classes
      override def decodeMany(o: Int, arr: Chunk[Object]): EitherNel[String, T2] = self.decodeMany(o, arr).map(f)
    }
  final def emap[T2](f: T => EitherNel[String, T2]): QueryDecoderMany[T2] =
    new QueryDecoderMany[T2] {
      override lazy val width: Int = self.width
      override lazy val classes: Chunk[Option[Class[?]]] = self.classes
      override def decodeMany(o: Int, arr: Chunk[Object]): EitherNel[String, T2] = self.decodeMany(o, arr).flatMap(f)
    }

  final def ~[T2](other: QueryDecoderMany[T2])(implicit z: Zip[T, T2]): QueryDecoderMany[z.Out] =
    new QueryDecoderMany[z.Out] {
      override lazy val width: Int = self.width + other.width
      override lazy val classes: Chunk[Option[Class[?]]] = self.classes ++ other.classes
      override def decodeMany(o: Int, arr: Chunk[Object]): EitherNel[String, z.Out] =
        for {
          a <- self.decodeMany(o, arr)
          b <- other.decodeMany(o + self.width, arr)
        } yield z.zip(a, b)
    }

  final def optional: QueryDecoderMany[Option[T]] =
    new QueryDecoderMany[Option[T]] {
      override lazy val width: Int = self.width
      override lazy val classes: Chunk[Option[Class[?]]] = self.classes
      override def decodeMany(o: Int, arr: Chunk[Object]): EitherNel[String, Option[T]] =
        if (o.until(o + width).forall(arr(_) == null)) None.asRight
        else self.decodeMany(o, arr).map(_.some)
    }

}
object QueryDecoderMany {

  implicit def fromSingle[T](implicit decoder: QueryDecoderSingle[T]): QueryDecoderMany[T] =
    new QueryDecoderMany[T] {
      override lazy val width: Int = 1
      override def decodeMany(o: Int, arr: Chunk[Object]): EitherNel[String, T] = decoder.decodeSingle(arr(o))
      override lazy val classes: Chunk[Option[Class[?]]] = Chunk(decoder.klass)
    }

  def fromCol[T](col: Col[T]): QueryDecoderMany[T] = col.codec.decoder

  inline def forTable[T[_[_]] <: Table](cols: T[Col])(implicit gen: K11.ProductGeneric[T]): QueryDecoderMany[T[K11.Identity]] =
    new QueryDecoderMany[T[K11.Identity]] {
      lazy val colsChunk: Chunk[Col[?]] = Chunk.fromArray { gen.toRepr(cols).toArray.map(_.asInstanceOf[Col[?]]) }
      override lazy val width: Int = colsChunk.length
      override lazy val classes: Chunk[Option[Class[?]]] = colsChunk.map(_.codec.decoder.klass)
      override def decodeMany(o: Int, arr: Chunk[Object]): EitherNel[String, T[K11.Identity]] = {
        @tailrec
        def loop(i: Int, o: Int, stop: Int, rStack: List[Any]): EitherNel[String, T[K11.Identity]] =
          if (o < stop) {
            val col = colsChunk(i)
            col.codec.decoder.decodeSingle(arr(o)) match {
              case Right(value) => loop(i + 1, o + 1, stop, value :: rStack)
              case Left(errors) => errors.map(e => s"Error decoding col ${col.colName}: $e").asLeft
            }
          } else gen.fromRepr(Tuple.fromIArray(IArray.from(rStack.reverse)).asInstanceOf).asRight

        loop(0, o, o + width, Nil)
      }
    }

}
