package harness.sql

import cats.data.{EitherNel, NonEmptyList}
import cats.syntax.either.*
import java.time.*
import java.util.UUID
import zio.json.JsonCodec

final case class ColCodec[T](encoder: ColEncoder[T], decoder: ColDecoder[T]) {

  def encodeColumn(t: T): ColT = encoder.encodeColumn(t)
  def decodeColumn(c: ColT): EitherNel[String, T] = decoder.decodeColumn(c)

  def imap[T2](mf: T => T2)(cmf: T2 => T): ColCodec[T2] = ColCodec(encoder.cmap(cmf), decoder.map(mf))
  def iemap[T2](mf: T => EitherNel[String, T2])(cmf: T2 => T): ColCodec[T2] = ColCodec(encoder.cmap(cmf), decoder.emap(mf))

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

  val date: ColCodec[LocalDate] = ColCodec(ColEncoder.date, ColDecoder.date)
  val time: ColCodec[LocalTime] = ColCodec(ColEncoder.time, ColDecoder.time)
  val dateTime: ColCodec[LocalDateTime] = ColCodec(ColEncoder.dateTime, ColDecoder.dateTime)

  def json[T](implicit codec: JsonCodec[T]): ColCodec[T] = ColCodec(ColEncoder.json[T], ColDecoder.json[T])

}
