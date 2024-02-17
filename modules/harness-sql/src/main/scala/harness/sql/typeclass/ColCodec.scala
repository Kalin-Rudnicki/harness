package harness.sql.typeclass

import cats.data.EitherNel
import cats.syntax.either.*
import harness.sql.*
import java.time.*
import java.util.UUID
import zio.json.JsonCodec

final case class ColCodec[T](encoder: ColEncoder[T], decoder: ColDecoder[T]) { self =>

  def imap[T2](mf: T => T2)(cmf: T2 => T): ColCodec[T2] = ColCodec(encoder.cmap(cmf), decoder.map(mf))
  def iemap[T2](mf: T => EitherNel[String, T2])(cmf: T2 => T): ColCodec[T2] = ColCodec(encoder.cmap(cmf), decoder.emap(mf))

  def optional: ColCodec[Option[T]] = ColCodec(encoder.optional, decoder.optional)

}
object ColCodec {

  val string: ColCodec[String] = ColCodec(ColEncoder.string, ColDecoder.string)
  val uuid: ColCodec[UUID] = ColCodec(ColEncoder.uuid, ColDecoder.uuid)
  val boolean: ColCodec[Boolean] = ColCodec(ColEncoder.boolean, ColDecoder.boolean)

  val short: ColCodec[Short] = ColCodec(ColEncoder.short, ColDecoder.short)
  val int: ColCodec[Int] = ColCodec(ColEncoder.int, ColDecoder.int)
  val long: ColCodec[Long] = ColCodec(ColEncoder.long, ColDecoder.long)

  val float: ColCodec[Float] = ColCodec(ColEncoder.float, ColDecoder.float)
  val double: ColCodec[Double] = ColCodec(ColEncoder.double, ColDecoder.double)

  val localDate: ColCodec[LocalDate] = ColCodec(ColEncoder.localDate, ColDecoder.localDate)
  val localTime: ColCodec[LocalTime] = ColCodec(ColEncoder.localTime, ColDecoder.localTime)
  val localDateTime: ColCodec[LocalDateTime] = ColCodec(ColEncoder.localDateTime, ColDecoder.localDateTime)

  def json[T](implicit codec: JsonCodec[T]): ColCodec[T] = ColCodec(ColEncoder.json[T], ColDecoder.json[T])

}
