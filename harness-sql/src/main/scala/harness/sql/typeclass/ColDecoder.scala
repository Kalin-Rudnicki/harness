package harness.sql.typeclass

import cats.data.{EitherNel, NonEmptyList}
import cats.syntax.either.*
import cats.syntax.option.*
import harness.sql.*
import java.sql.ResultSet
import java.time.*
import java.util.UUID
import org.postgresql.util.PGobject
import scala.util.Try
import zio.json.JsonDecoder
import zio.json.ast.Json

trait ColDecoder[T] { self =>

  def decodeColumn(c: Object): EitherNel[String, T]
  val jsonDecoder: JsonDecoder[T]

  final def map[T2](f: T => T2): ColDecoder[T2] =
    new ColDecoder[T2] {
      override def decodeColumn(c: Object): EitherNel[String, T2] = self.decodeColumn(c).map(f)
      override val jsonDecoder: JsonDecoder[T2] = self.jsonDecoder.map(f)
    }
  final def emap[T2](f: T => EitherNel[String, T2]): ColDecoder[T2] =
    new ColDecoder[T2] {
      override def decodeColumn(c: Object): EitherNel[String, T2] = self.decodeColumn(c).flatMap(f)
      override val jsonDecoder: JsonDecoder[T2] = self.jsonDecoder.mapOrFail(f(_).leftMap(_.head))
    }

  final def optional: ColDecoder[Option[T]] =
    new ColDecoder[Option[T]] {
      override def decodeColumn(c: Object): EitherNel[String, Option[T]] =
        Option(c) match {
          case Some(c) => self.decodeColumn(c).map(_.some)
          case None    => None.asRight
        }
      override val jsonDecoder: JsonDecoder[Option[T]] = JsonDecoder.option[T](self.jsonDecoder)
    }

}
object ColDecoder {

  private def unsafeDecode[T: JsonDecoder](pf: PartialFunction[Object, T]): ColDecoder[T] =
    new ColDecoder[T] {
      override def decodeColumn(c: Object): EitherNel[String, T] = pf(c).asRight
      override val jsonDecoder: JsonDecoder[T] = JsonDecoder[T]
    }

  private def unsafeDecodeE[T: JsonDecoder](pf: PartialFunction[Object, EitherNel[String, T]]): ColDecoder[T] =
    new ColDecoder[T] {
      override def decodeColumn(c: Object): EitherNel[String, T] = pf(c)
      override val jsonDecoder: JsonDecoder[T] = JsonDecoder[T]
    }

  val string: ColDecoder[String] = unsafeDecode { case string: String => string }
  val uuid: ColDecoder[UUID] = unsafeDecode { case uuid: UUID => uuid }
  val boolean: ColDecoder[Boolean] = unsafeDecode { case boolean: java.lang.Boolean => boolean }

  val short: ColDecoder[Short] = unsafeDecode { case short: java.lang.Short => short }
  val int: ColDecoder[Int] = unsafeDecode { case int: java.lang.Integer => int }
  val long: ColDecoder[Long] = unsafeDecode { case long: java.lang.Long => long }

  val float: ColDecoder[Float] = unsafeDecode { case float: java.lang.Float => float }
  val double: ColDecoder[Double] = unsafeDecode { case double: java.lang.Double => double }

  val date: ColDecoder[LocalDate] = unsafeDecode { case date: java.sql.Date => date.toLocalDate }
  val time: ColDecoder[LocalTime] = unsafeDecode { case time: java.sql.Time => time.toLocalTime }
  val dateTime: ColDecoder[LocalDateTime] = unsafeDecode { case dateTime: java.sql.Timestamp => dateTime.toLocalDateTime }

  def json[T](implicit codec: JsonDecoder[T]): ColDecoder[T] =
    unsafeDecodeE {
      case string: String => codec.decodeJson(string).leftMap(NonEmptyList.one)
      case obj: PGobject  => codec.decodeJson(obj.toString).leftMap(NonEmptyList.one)
    }

}
