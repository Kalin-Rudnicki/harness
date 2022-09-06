package harness.sql

import cats.data.{EitherNel, NonEmptyList}
import cats.syntax.either.*
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
  private def unimplemented[T]: ColDecoder[T] =
    _ => ??? // TODO (KR) :

  val string: ColDecoder[String] = unimplemented // TODO (KR) :
  val uuid: ColDecoder[UUID] = unimplemented // TODO (KR) :
  val boolean: ColDecoder[Boolean] = unimplemented // TODO (KR) :

  val short: ColDecoder[Short] = unimplemented // TODO (KR) :
  val int: ColDecoder[Int] = unimplemented // TODO (KR) :
  val long: ColDecoder[Long] = unimplemented // TODO (KR) :
  
  val float: ColDecoder[Float] = unimplemented // TODO (KR) :
  val double: ColDecoder[Double] = unimplemented // TODO (KR) :

  val date: ColDecoder[LocalDate] = unimplemented // TODO (KR) :
  val time: ColDecoder[LocalTime] = unimplemented // TODO (KR) :
  val dateTime: ColDecoder[LocalDateTime] = unimplemented // TODO (KR) :

  def json[T](implicit codec: JsonDecoder[T]): ColDecoder[T] =
    codec.decodeJson(_).leftMap(NonEmptyList.one)

}
