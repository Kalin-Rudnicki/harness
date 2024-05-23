package harness.sql.typeclass

import cats.data.EitherNel
import cats.syntax.option.*
import harness.sql.*
import java.time.*
import java.util.UUID
import zio.json.*

final case class QueryCodecSingle[T](encoder: QueryEncoderSingle[T], decoder: QueryDecoderSingle[T]) { self =>

  def imap[T2](mf: T => T2)(cmf: T2 => T): QueryCodecSingle[T2] = QueryCodecSingle(encoder.cmap(cmf), decoder.map(mf))
  def iemap[T2](mf: T => EitherNel[String, T2])(cmf: T2 => T): QueryCodecSingle[T2] = QueryCodecSingle(encoder.cmap(cmf), decoder.emap(mf))

  def optional: QueryCodecSingle[Option[T]] = QueryCodecSingle(encoder.optional, decoder.optional)

}
object QueryCodecSingle {

  // =====|  |=====

  implicit val string: QueryCodecSingle[String] =
    QueryCodecSingle(
      QueryEncoderSingle.builder.withoutClass,
      QueryDecoderSingle.builder.unsafeDecode() { case string: String => string },
    )
  implicit val uuid: QueryCodecSingle[UUID] =
    QueryCodecSingle(
      QueryEncoderSingle.builder.withoutClass,
      QueryDecoderSingle.builder.unsafeDecode() { case uuid: UUID => uuid },
    )
  implicit val boolean: QueryCodecSingle[Boolean] =
    QueryCodecSingle(
      QueryEncoderSingle.builder.withoutClass[java.lang.Boolean].cmap[Boolean](java.lang.Boolean.valueOf),
      QueryDecoderSingle.builder.unsafeDecode() { case boolean: java.lang.Boolean => boolean },
    )

  implicit val short: QueryCodecSingle[Short] =
    QueryCodecSingle(
      QueryEncoderSingle.builder.withoutClass[java.lang.Short].cmap[Short](java.lang.Short.valueOf),
      QueryDecoderSingle.builder.unsafeDecode() { case short: java.lang.Short => short },
    )
  implicit val int: QueryCodecSingle[Int] =
    QueryCodecSingle(
      QueryEncoderSingle.builder.withoutClass[java.lang.Integer].cmap[Int](java.lang.Integer.valueOf),
      QueryDecoderSingle.builder.unsafeDecode() { case int: java.lang.Integer => int },
    )
  implicit val long: QueryCodecSingle[Long] =
    QueryCodecSingle(
      QueryEncoderSingle.builder.withoutClass[java.lang.Long].cmap[Long](java.lang.Long.valueOf),
      QueryDecoderSingle.builder.unsafeDecode() { case long: java.lang.Long => long },
    )

  implicit val float: QueryCodecSingle[Float] =
    QueryCodecSingle(
      QueryEncoderSingle.builder.withoutClass[java.lang.Float].cmap[Float](java.lang.Float.valueOf),
      QueryDecoderSingle.builder.unsafeDecode() { case float: java.lang.Float => float },
    )
  implicit val double: QueryCodecSingle[Double] =
    QueryCodecSingle(
      QueryEncoderSingle.builder.withoutClass[java.lang.Double].cmap[Double](java.lang.Double.valueOf),
      QueryDecoderSingle.builder.unsafeDecode() { case double: java.lang.Double => double },
    )

  implicit val localDate: QueryCodecSingle[LocalDate] =
    QueryCodecSingle(
      QueryEncoderSingle.builder.withClass,
      QueryDecoderSingle.builder.unsafeDecode(classOf[LocalDate].some) { case date: LocalDate => date },
    )
  implicit val localTime: QueryCodecSingle[LocalTime] =
    QueryCodecSingle(
      QueryEncoderSingle.builder.withClass,
      QueryDecoderSingle.builder.unsafeDecode(classOf[LocalTime].some) { case time: LocalTime => time },
    )
  implicit val localDateTime: QueryCodecSingle[LocalDateTime] =
    QueryCodecSingle(
      QueryEncoderSingle.builder.withClass,
      QueryDecoderSingle.builder.unsafeDecode(classOf[LocalDateTime].some) { case dateTime: LocalDateTime => dateTime },
    )

  implicit val offsetDateTime: QueryCodecSingle[OffsetDateTime] =
    QueryCodecSingle(
      QueryEncoderSingle.builder.withClass,
      QueryDecoderSingle.builder.unsafeDecode(classOf[OffsetDateTime].some) { case dateTime: OffsetDateTime => dateTime },
    )

  // =====|  |=====

  val json: QueryCodecSingle[String] = QueryCodecSingle(QueryEncoderSingle.json, QueryDecoderSingle.json)
  val jsonb: QueryCodecSingle[String] = QueryCodecSingle(QueryEncoderSingle.jsonb, QueryDecoderSingle.jsonb)
  def encodedJson[T: JsonCodec]: QueryCodecSingle[T] = QueryCodecSingle(QueryEncoderSingle.encodedJson[T], QueryDecoderSingle.encodedJson[T])
  def encodedJsonb[T: JsonCodec]: QueryCodecSingle[T] = QueryCodecSingle(QueryEncoderSingle.encodedJsonb[T], QueryDecoderSingle.encodedJsonb[T])

}
