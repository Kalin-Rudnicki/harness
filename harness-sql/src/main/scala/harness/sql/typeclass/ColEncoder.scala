package harness.sql.typeclass

import harness.sql.*
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
  private def fromStringEncoder[T](implicit enc: harness.core.StringEncoder[T]): ColEncoder[T] = enc.encode(_)

  val string: ColEncoder[String] = fromStringEncoder // TODO (KR) :
  val uuid: ColEncoder[UUID] = fromStringEncoder // TODO (KR) :
  val boolean: ColEncoder[Boolean] = fromStringEncoder // TODO (KR) :

  val short: ColEncoder[Short] = fromStringEncoder(using harness.core.StringEncoder.usingToString) // TODO (KR) :
  val int: ColEncoder[Int] = fromStringEncoder // TODO (KR) :
  val long: ColEncoder[Long] = fromStringEncoder // TODO (KR) :

  val float: ColEncoder[Float] = fromStringEncoder // TODO (KR) :
  val double: ColEncoder[Double] = fromStringEncoder // TODO (KR) :

  val date: ColEncoder[LocalDate] = fromStringEncoder(using harness.core.StringEncoder.usingToString) // TODO (KR) :
  val time: ColEncoder[LocalTime] = fromStringEncoder(using harness.core.StringEncoder.usingToString) // TODO (KR) :
  val dateTime: ColEncoder[LocalDateTime] = fromStringEncoder(using harness.core.StringEncoder.usingToString) // TODO (KR) :

  def json[T](implicit codec: JsonEncoder[T]): ColEncoder[T] =
    t => codec.encodeJson(t, None).toString

}
