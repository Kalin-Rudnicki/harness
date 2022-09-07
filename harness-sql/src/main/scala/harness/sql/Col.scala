package harness.sql

import harness.sql.typeclass.ColCodec

import java.time.*
import java.util.UUID
import zio.json.JsonCodec

final class Col[T] private (
    val colName: String,
    val colType: String, // TODO (KR) :
    val colCodec: ColCodec[T],
) {

  final def imap[T2](mf: T => T2)(cmf: T2 => T): Col[T2] =
    Col(colName, colType, colCodec.imap(mf)(cmf))

  override def toString: String = s"$colName[$colType]"

}
object Col {

  final case class Name(name: String) extends scala.annotation.Annotation

  def string(name: String): Col[String] = Col(name, "TEXT", ColCodec.string)
  def uuid(name: String): Col[UUID] = Col(name, "UUID", ColCodec.uuid)
  def boolean(name: String): Col[Boolean] = Col(name, "BOOLEAN", ColCodec.boolean)

  def short(name: String): Col[Short] = Col(name, "SMALLINT", ColCodec.short)
  def int(name: String): Col[Int] = Col(name, "INTEGER", ColCodec.int)
  def long(name: String): Col[Long] = Col(name, "BIGINT", ColCodec.long)

  def float(name: String): Col[Float] = Col(name, "REAL", ColCodec.float)
  def double(name: String): Col[Double] = Col(name, "DOUBLE PRECISION", ColCodec.double)

  def date(name: String): Col[LocalDate] = Col(name, "DATE", ColCodec.date)
  def time(name: String): Col[LocalTime] = Col(name, "TIME", ColCodec.time)
  def dateTime(name: String): Col[LocalDateTime] = Col(name, "TIMESTAMP", ColCodec.dateTime)

  def json[T: JsonCodec](name: String): Col[T] = Col(name, "JSON", ColCodec.json[T])
  def jsonb[T: JsonCodec](name: String): Col[T] = Col(name, "JSONB", ColCodec.json[T])

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
