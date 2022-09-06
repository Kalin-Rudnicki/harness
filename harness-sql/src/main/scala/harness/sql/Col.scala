package harness.sql

import harness.sql.typeclass.*
import java.time.*
import java.util.UUID
import zio.json.JsonCodec

final case class Col[T] private (
    colName: String,
    colType: String,
    colCodec: ColCodec[T],
    nullable: Boolean,
) {

  def imap[T2](mf: T => T2)(cmf: T2 => T): Col[T2] =
    Col(colName, colType, colCodec.imap(mf)(cmf), nullable)

  def optional: Col[Option[T]] =
    Col(colName, colType, colCodec.optional, true)

  override def toString: String = s"$colName[$colType]"

}
object Col {

  final case class Name(name: String) extends scala.annotation.Annotation

  def string(name: String): Col[String] = Col(name, "TEXT", ColCodec.string, false)
  def uuid(name: String): Col[UUID] = Col(name, "UUID", ColCodec.uuid, false)
  def boolean(name: String): Col[Boolean] = Col(name, "BOOLEAN", ColCodec.boolean, false)

  def short(name: String): Col[Short] = Col(name, "SMALLINT", ColCodec.short, false)
  def int(name: String): Col[Int] = Col(name, "INTEGER", ColCodec.int, false)
  def long(name: String): Col[Long] = Col(name, "BIGINT", ColCodec.long, false)

  def float(name: String): Col[Float] = Col(name, "REAL", ColCodec.float, false)
  def double(name: String): Col[Double] = Col(name, "DOUBLE PRECISION", ColCodec.double, false)

  def date(name: String): Col[LocalDate] = Col(name, "DATE", ColCodec.date, false)
  def time(name: String): Col[LocalTime] = Col(name, "TIME", ColCodec.time, false)
  def dateTime(name: String): Col[LocalDateTime] = Col(name, "TIMESTAMP", ColCodec.dateTime, false)

  def json[T: JsonCodec](name: String): Col[T] = Col(name, "JSON", ColCodec.json[T], false)
  def jsonb[T: JsonCodec](name: String): Col[T] = Col(name, "JSONB", ColCodec.json[T], false)

  trait GenCol[T] {
    def make(name: String): Col[T]
  }
  object GenCol {

    given GenCol[String] = Col.string(_)
    given GenCol[UUID] = Col.uuid(_)
    given GenCol[Boolean] = Col.boolean(_)

    given GenCol[Short] = Col.short(_)
    given GenCol[Int] = Col.int(_)
    given GenCol[Long] = Col.long(_)

    given GenCol[Float] = Col.float(_)
    given GenCol[Double] = Col.double(_)

    given GenCol[LocalDate] = Col.date(_)
    given GenCol[LocalTime] = Col.time(_)
    given GenCol[LocalDateTime] = Col.dateTime(_)

  }

}
