package harness.sql

import java.time.*
import java.util.UUID
import zio.json.JsonEncoder

trait ColEncoder[T] { self =>

  def encodeColumn(t: T): ColT

  final def cmap[T2](f: T2 => T): ColEncoder[T2] =
    t => self.encodeColumn(f(t))

}
object ColEncoder {

  // TODO (KR) : REMOVE
  private def unimplemented[T]: ColEncoder[T] =
    _.toString

  val string: ColEncoder[String] = unimplemented // TODO (KR) :
  val uuid: ColEncoder[UUID] = unimplemented // TODO (KR) :
  val boolean: ColEncoder[Boolean] = unimplemented // TODO (KR) :

  val short: ColEncoder[Short] = unimplemented // TODO (KR) :
  val int: ColEncoder[Int] = unimplemented // TODO (KR) :
  val long: ColEncoder[Long] = unimplemented // TODO (KR) :
  
  val float: ColEncoder[Float] = unimplemented // TODO (KR) :
  val double: ColEncoder[Double] = unimplemented // TODO (KR) :

  val date: ColEncoder[LocalDate] = unimplemented // TODO (KR) :
  val time: ColEncoder[LocalTime] = unimplemented // TODO (KR) :
  val dateTime: ColEncoder[LocalDateTime] = unimplemented // TODO (KR) :

  def json[T](implicit codec: JsonEncoder[T]): ColEncoder[T] =
    t => codec.encodeJson(t, None).toString

}
