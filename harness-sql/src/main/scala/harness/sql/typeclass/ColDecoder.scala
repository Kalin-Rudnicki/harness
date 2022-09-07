package harness.sql.typeclass

import cats.data.{EitherNel, NonEmptyList}
import cats.syntax.either.*
import harness.sql.*
import java.time.*
import java.util.UUID
import zio.json.JsonDecoder

trait ColDecoder[T] { self =>

  def decodeColumn(c: ColT): EitherNel[String, T]

  final def map[T2](f: T => T2): ColDecoder[T2] = self.decodeColumn(_).map(f)
  final def emap[T2](f: T => EitherNel[String, T2]): ColDecoder[T2] = self.decodeColumn(_).flatMap(f)

}
object ColDecoder {

  // TODO (KR) : REMOVE
  private def fromStringDecoder[T](implicit dec: harness.core.StringDecoder[T]): ColDecoder[T] =
    dec.decodeAccumulating(_)

  val string: ColDecoder[String] = fromStringDecoder // TODO (KR) :
  val uuid: ColDecoder[UUID] = fromStringDecoder // TODO (KR) :
  val boolean: ColDecoder[Boolean] = fromStringDecoder // TODO (KR) :

  val short: ColDecoder[Short] = fromStringDecoder(using harness.core.StringDecoder.fromOptionF("Short", _.toShortOption)) // TODO (KR) :
  val int: ColDecoder[Int] = fromStringDecoder // TODO (KR) :
  val long: ColDecoder[Long] = fromStringDecoder // TODO (KR) :

  val float: ColDecoder[Float] = fromStringDecoder // TODO (KR) :
  val double: ColDecoder[Double] = fromStringDecoder // TODO (KR) :

  val date: ColDecoder[LocalDate] = fromStringDecoder // TODO (KR) :
  val time: ColDecoder[LocalTime] = fromStringDecoder // TODO (KR) :
  val dateTime: ColDecoder[LocalDateTime] = fromStringDecoder // TODO (KR) :

  def json[T](implicit codec: JsonDecoder[T]): ColDecoder[T] =
    codec.decodeJson(_).leftMap(NonEmptyList.one)

}
