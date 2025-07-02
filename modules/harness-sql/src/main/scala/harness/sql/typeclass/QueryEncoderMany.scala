package harness.sql.typeclass

import harness.core.Zip
import harness.deriving.*
import harness.sql.*
import harness.sql.typeclass.*
import zio.Chunk

trait QueryEncoderMany[T] { self =>

  lazy val klasses: Chunk[Option[Class[?]]]
  final lazy val width: Int = klasses.length
  def encodeMany(t: T): Chunk[Object]

  final def cmap[T2](f: T2 => T): QueryEncoderMany[T2] =
    new QueryEncoderMany[T2] {
      override lazy val klasses: Chunk[Option[Class[?]]] = self.klasses
      override def encodeMany(t: T2): Chunk[Object] = self.encodeMany(f(t))
    }

  final def ~[T2](other: QueryEncoderMany[T2])(implicit z: Zip[T, T2]): QueryEncoderMany[z.Out] =
    new QueryEncoderMany[z.Out] {
      override lazy val klasses: Chunk[Option[Class[?]]] = self.klasses ++ other.klasses
      override def encodeMany(t: z.Out): Chunk[Object] = {
        val (a, b) = z.unzip(t)
        self.encodeMany(a) ++ other.encodeMany(b)
      }
    }

  final def optional: QueryEncoderMany[Option[T]] =
    new QueryEncoderMany[Option[T]] {
      override lazy val klasses: Chunk[Option[Class[?]]] = self.klasses
      override def encodeMany(t: Option[T]): Chunk[Object] = t match
        case Some(value) => self.encodeMany(value)
        case None        => Chunk.fill(width)(null.asInstanceOf[Object])
    }

}
object QueryEncoderMany {

  implicit def fromSingle[T](implicit encoder: QueryEncoderSingle[T]): QueryEncoderMany[T] =
    new QueryEncoderMany[T] {
      override lazy val klasses: Chunk[Option[Class[?]]] = Chunk.single(encoder.klass)
      override def encodeMany(t: T): Chunk[Object] = Chunk.single(encoder.encodeSingle(t))
    }

  @scala.annotation.nowarn
  inline def forTable[T[_[_]] <: Table](encoders: T[QueryEncoderSingle])(implicit flatten: Flatten[T]): QueryEncoderMany[T[K11.Identity]] =
    new QueryEncoderMany[T[K11.Identity]] {
      lazy val encodersChunk: Chunk[QueryEncoderSingle[?]] = flatten(encoders)
      override lazy val klasses: Chunk[Option[Class[?]]] = encodersChunk.map(_.klass)
      override def encodeMany(t: T[K11.Identity]): Chunk[Object] = flatten(t).zipWith(encodersChunk) { (t, i) => i.encodeSingle(t.asInstanceOf) }
    }

  @scala.annotation.nowarn
  inline def forTableFiltered[T[_[_]] <: Table](encoders: T[QueryEncoderSingle], filter: T[K11.Const[Boolean]])(implicit flatten: Flatten[T]): QueryEncoderMany[T[K11.Identity]] =
    new QueryEncoderMany[T[K11.Identity]] {
      lazy val encodersChunk: Chunk[QueryEncoderSingle[?]] = flatten(encoders)
      lazy val filterChunk: Chunk[Boolean] = flatten(filter)
      lazy val zippedChunk: Chunk[(QueryEncoderSingle[?], Boolean)] = encodersChunk.zip(filterChunk)
      override lazy val klasses: Chunk[Option[Class[?]]] = zippedChunk.collect { case (enc, true) => enc.klass }
      override def encodeMany(t: T[K11.Identity]): Chunk[Object] = flatten(t).zip { zippedChunk }.collect { case (t, (i, true)) => i.encodeSingle(t.asInstanceOf) }
    }

}
