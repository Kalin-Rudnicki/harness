package harness.core

import java.time.*
import java.util.UUID

trait StringEncoder[-T] {

  def encode(t: T): String

  final def imap[T2](f: T2 => T): StringEncoder[T2] =
    t2 => encode(f(t2))

}
object StringEncoder {

  def apply[T: StringEncoder]: StringEncoder[T] =
    implicitly[StringEncoder[T]]

  def usingToString[T]: StringEncoder[T] = _.toString

  implicit val string: StringEncoder[String] = identity(_)

  implicit val boolean: StringEncoder[Boolean] = usingToString

  implicit val int: StringEncoder[Int] = usingToString

  implicit val long: StringEncoder[Long] = usingToString

  implicit val bigInt: StringEncoder[BigInt] = usingToString

  implicit val float: StringEncoder[Float] = usingToString

  implicit val double: StringEncoder[Double] = usingToString

  implicit val bigDecimal: StringEncoder[BigDecimal] = usingToString

  implicit val uuid: StringEncoder[UUID] = usingToString

  implicit val duration: StringEncoder[Duration] = usingToString

  implicit val localDate: StringEncoder[LocalDate] = usingToString

  implicit val localTime: StringEncoder[LocalTime] = usingToString

  implicit val localDateTime: StringEncoder[LocalDateTime] = usingToString

  def list[T](sep: String)(implicit tEncoder: StringEncoder[T]): StringEncoder[List[T]] =
    ts => ts.map(tEncoder.encode).mkString(sep)

  implicit def list[T: StringEncoder]: StringEncoder[List[T]] =
    list[T](",")

}
