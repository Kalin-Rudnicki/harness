package harness.sql

import cats.data.{EitherNel, NonEmptyList}
import cats.syntax.either.*
import java.time.*
import java.util.UUID
import zio.json.JsonCodec

trait ColCodec[T] { self =>

  def encodeColumn(t: T): ColT
  def decodeColumn(s: ColT): EitherNel[String, T]

  final def imap[T2](mf: T => T2)(cmf: T2 => T): ColCodec[T2] =
    new ColCodec[T2] {
      override def encodeColumn(t: T2): ColT = self.encodeColumn(cmf(t))
      override def decodeColumn(s: ColT): EitherNel[String, T2] = self.decodeColumn(s).map(mf)
    }

  final def optional: ColCodec[Option[T]] =
    new ColCodec[Option[T]] {
      override def encodeColumn(t: Option[T]): ColT = ??? // TODO (KR) :
      override def decodeColumn(s: ColT): EitherNel[String, Option[T]] = ??? // TODO (KR) :
    }

}
object ColCodec {

  // TODO (KR) : REMOVE
  private def unimplemented[T]: ColCodec[T] =
    new ColCodec[T] {
      override def encodeColumn(t: T): ColT = t.toString
      override def decodeColumn(s: ColT): EitherNel[String, T] = ???
    }

  val string: ColCodec[String] = unimplemented // TODO (KR) :
  val uuid: ColCodec[UUID] = unimplemented // TODO (KR) :
  val boolean: ColCodec[Boolean] = unimplemented // TODO (KR) :

  val short: ColCodec[Short] = unimplemented // TODO (KR) :
  val int: ColCodec[Int] = unimplemented // TODO (KR) :
  val long: ColCodec[Long] = unimplemented // TODO (KR) :
  val float: ColCodec[Float] = unimplemented // TODO (KR) :
  val double: ColCodec[Double] = unimplemented // TODO (KR) :

  val date: ColCodec[LocalDate] = unimplemented // TODO (KR) :
  val time: ColCodec[LocalTime] = unimplemented // TODO (KR) :
  val dateTime: ColCodec[LocalDateTime] = unimplemented // TODO (KR) :

  def json[T](implicit codec: JsonCodec[T]): ColCodec[T] =
    new ColCodec[T] {
      override def encodeColumn(t: T): ColT = codec.encodeJson(t, None).toString
      override def decodeColumn(s: ColT): EitherNel[String, T] = codec.decodeJson(s).leftMap(NonEmptyList.one)
    }

}
