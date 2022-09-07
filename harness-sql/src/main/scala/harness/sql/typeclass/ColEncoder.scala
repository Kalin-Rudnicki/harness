package harness.sql.typeclass

import harness.sql.*
import java.sql.{PreparedStatement, Types}
import java.time.*
import java.util.UUID
import zio.json.JsonEncoder

trait ColEncoder[T] { self =>

  def encodeColumn(t: T): Object

  final def cmap[T2](f: T2 => T): ColEncoder[T2] =
    t => self.encodeColumn(f(t))

  final def optional: ColEncoder[Option[T]] = {
    case Some(value) => self.encodeColumn(value)
    case None        => null
  }

}
object ColEncoder {

  val string: ColEncoder[String] = t => t
  val uuid: ColEncoder[UUID] = t => t
  val boolean: ColEncoder[Boolean] = java.lang.Boolean.valueOf(_)

  val short: ColEncoder[Short] = java.lang.Short.valueOf(_)
  val int: ColEncoder[Int] = java.lang.Integer.valueOf(_)
  val long: ColEncoder[Long] = java.lang.Long.valueOf(_)

  val float: ColEncoder[Float] = java.lang.Float.valueOf(_)
  val double: ColEncoder[Double] = java.lang.Double.valueOf(_)

  val date: ColEncoder[LocalDate] = java.sql.Date.valueOf(_)
  val time: ColEncoder[LocalTime] = java.sql.Time.valueOf(_)
  val dateTime: ColEncoder[LocalDateTime] = java.sql.Timestamp.valueOf(_)

  def json[T](implicit codec: JsonEncoder[T]): ColEncoder[T] =
    t => codec.encodeJson(t, None).toString

}
