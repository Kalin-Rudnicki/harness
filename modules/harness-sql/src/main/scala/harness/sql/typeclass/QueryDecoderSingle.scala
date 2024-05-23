package harness.sql.typeclass

import cats.data.{EitherNel, NonEmptyList}
import cats.syntax.either.*
import cats.syntax.option.*
import org.postgresql.util.PGobject
import zio.json.*

trait QueryDecoderSingle[T] { self =>

  def decodeSingle(c: Object): EitherNel[String, T]
  val jsonDecoder: JsonDecoder[T]
  val klass: Option[Class[?]]

  final def map[T2](f: T => T2): QueryDecoderSingle[T2] =
    new QueryDecoderSingle[T2] {
      override def decodeSingle(c: Object): EitherNel[String, T2] = self.decodeSingle(c).map(f)
      override val jsonDecoder: JsonDecoder[T2] = self.jsonDecoder.map(f)
      override val klass: Option[Class[?]] = self.klass
    }
  final def emap[T2](f: T => EitherNel[String, T2]): QueryDecoderSingle[T2] =
    new QueryDecoderSingle[T2] {
      override def decodeSingle(c: Object): EitherNel[String, T2] = self.decodeSingle(c).flatMap(f)
      override val jsonDecoder: JsonDecoder[T2] = self.jsonDecoder.mapOrFail(f(_).leftMap(_.head))
      override val klass: Option[Class[?]] = self.klass
    }

  final def optional: QueryDecoderSingle[Option[T]] =
    new QueryDecoderSingle[Option[T]] {
      override def decodeSingle(c: Object): EitherNel[String, Option[T]] =
        Option(c) match {
          case Some(c) => self.decodeSingle(c).map(_.some)
          case None    => None.asRight
        }
      override val jsonDecoder: JsonDecoder[Option[T]] = JsonDecoder.option[T](self.jsonDecoder)
      override val klass: Option[Class[?]] = self.klass
    }

  final def withJsonDecoder(jsonDecoder: JsonDecoder[T]): QueryDecoderSingle[T] =
    new QueryDecoderSingle[T] {
      override def decodeSingle(c: Object) = self.decodeSingle(c)
      override val jsonDecoder: JsonDecoder[T] = jsonDecoder
      override val klass: Option[Class[?]] = self.klass
    }

}
object QueryDecoderSingle {

  object builder {

    def unsafeDecode[T: JsonDecoder](k: Option[Class[?]] = None)(pf: PartialFunction[Object, T]): QueryDecoderSingle[T] =
      new QueryDecoderSingle[T] {
        override def decodeSingle(c: Object): EitherNel[String, T] = pf(c).asRight
        override val jsonDecoder: JsonDecoder[T] = JsonDecoder[T]
        override val klass: Option[Class[?]] = k
      }

  }

  implicit def fromCodec[T](implicit codec: QueryCodecSingle[T]): QueryDecoderSingle[T] = codec.decoder

  val json: QueryDecoderSingle[String] = builder.unsafeDecode() { case obj: PGobject => obj.getValue }
  val jsonb: QueryDecoderSingle[String] = builder.unsafeDecode() { case obj: PGobject => obj.getValue }

  def encodedJson[T](implicit codec: JsonDecoder[T]): QueryDecoderSingle[T] =
    json.emap(_.fromJson[T].leftMap(NonEmptyList.one)).withJsonDecoder(codec)
  def encodedJsonb[T](implicit codec: JsonDecoder[T]): QueryDecoderSingle[T] =
    jsonb.emap(_.fromJson[T].leftMap(NonEmptyList.one)).withJsonDecoder(codec)

}
