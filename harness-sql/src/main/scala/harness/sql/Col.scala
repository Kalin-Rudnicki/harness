package harness.sql

import cats.data.{EitherNel, NonEmptyList}
import cats.syntax.either.*
import cats.syntax.option.*
import harness.core.*
import harness.sql.typeclass.*
import java.time.*
import java.time.format.DateTimeFormatter
import java.util.{TimeZone, UUID}
import scala.reflect.ClassTag
import scala.util.Try
import zio.json.JsonCodec

final case class Col[T] private (
    colName: String,
    colType: String,
    colCodec: ColCodec[T],
    nullable: Boolean,
    constraints: List[Col.Constraint],
    setType: Option[String],
    getType: Option[String],
) {

  def imap[T2](mf: T => T2)(cmf: T2 => T): Col[T2] =
    Col(colName, colType, colCodec.imap(mf)(cmf), nullable, constraints, setType, getType)

  def iemap[T2](mf: T => EitherNel[String, T2])(cmf: T2 => T): Col[T2] =
    Col(colName, colType, colCodec.iemap(mf)(cmf), nullable, constraints, setType, getType)

  inline def imapTry[T2](mf: T => T2)(cmf: T2 => T): Col[T2] =
    iemap(t => Try(mf(t)).toEither.leftMap(e => NonEmptyList.one(e.toString)))(cmf)

  def optional: Col[Option[T]] =
    Col(colName, colType, colCodec.optional, true, constraints, setType, getType)

  def primaryKey: Col[T] =
    Col(colName, colType, colCodec, nullable, Col.Constraint.PrimaryKey :: constraints, setType, getType)
  def references(foreignKeyRef: => ForeignKeyRef): Col[T] =
    Col(colName, colType, colCodec, nullable, Col.Constraint.ForeignKey(foreignKeyRef) :: constraints, setType, getType)

  override def toString: String = s"$colName[$colType]"

  def ? : String = setType.fold("?")(t => s"? :: $t")
  def `(?)` : String = setType.fold("?")(t => s"(? :: $t)")

}
object Col {

  // =====| BuiltIn |=====

  private inline def basic[T](colName: String, colType: String, colCodec: ColCodec[T]): Col[T] = Col(colName, colType, colCodec, false, Nil, None, None)

  def string(name: String): Col[String] = Col.basic(name, "TEXT", ColCodec.string)
  def uuid(name: String): Col[UUID] = Col.basic(name, "UUID", ColCodec.uuid)
  def boolean(name: String): Col[Boolean] = Col.basic(name, "BOOLEAN", ColCodec.boolean)

  def short(name: String): Col[Short] = Col.basic(name, "SMALLINT", ColCodec.short)
  def int(name: String): Col[Int] = Col.basic(name, "INTEGER", ColCodec.int)
  def long(name: String): Col[Long] = Col.basic(name, "BIGINT", ColCodec.long)

  def float(name: String): Col[Float] = Col.basic(name, "REAL", ColCodec.float)
  def double(name: String): Col[Double] = Col.basic(name, "DOUBLE PRECISION", ColCodec.double)

  def localDate(name: String): Col[LocalDate] = Col.basic(name, "DATE", ColCodec.localDate)
  def localTime(name: String): Col[LocalTime] = Col.basic(name, "TIME", ColCodec.localTime)
  def localDateTime(name: String): Col[LocalDateTime] = Col.basic(name, "TIMESTAMP", ColCodec.localDateTime)

  def json[T: JsonCodec](name: String): Col[T] = Col(name, "JSON", ColCodec.json[T], false, Nil, "JSON".some, None)
  def jsonb[T: JsonCodec](name: String): Col[T] = Col(name, "JSONB", ColCodec.json[T], false, Nil, "JSONB".some, None)

  // =====| Helpers |=====

  def mappedEnum[E <: Enum[E], Enc](colName: String)(implicit ewe: Enum.WithEnc[E, Enc], gc: Col.GenCol[Enc], ct: ClassTag[E]): Col[E] =
    gc.make(colName).iemap { v => ewe.decode(v).toRight(NonEmptyList.one(s"Invalid ${ct.runtimeClass.getSimpleName}: $v")) }(ewe.encode)

  // NOTE : It would be nice if these could use the built-in PSQL 'TIME WITH TIME ZONE' and 'TIMESTAMP WITH TIME ZONE',
  //      : but the issue is that when PSQL stores 'TIMESTAMP WITH TIME ZONE', it stores it in UTC, and then gives it back to you in your time zone.
  //      : This is an issue if you care about
  def offsetTime(name: String, dtf: DateTimeFormatter = DateTimeFormatter.ISO_OFFSET_TIME): Col[OffsetTime] =
    Col.string(name).imapTry(OffsetTime.parse(_, dtf))(dtf.format)
  def offsetDateTime(name: String, dtf: DateTimeFormatter = DateTimeFormatter.ISO_OFFSET_DATE_TIME): Col[OffsetDateTime] =
    Col.string(name).imapTry(OffsetDateTime.parse(_, dtf))(dtf.format)

  def zonedDateTime(name: String, dtf: DateTimeFormatter = DateTimeFormatter.ISO_ZONED_DATE_TIME): Col[ZonedDateTime] =
    Col.string(name).imapTry(ZonedDateTime.parse(_, dtf))(dtf.format)

  def zoneOffset(name: String): Col[ZoneOffset] =
    Col.string(name).imapTry(ZoneOffset.of)(_.getId)
  def zoneId(name: String): Col[ZoneId] =
    Col.string(name).imapTry(ZoneId.of)(_.getId)
  def timeZone(name: String): Col[TimeZone] =
    Col.zoneId(name).imapTry(TimeZone.getTimeZone)(_.toZoneId)

  // =====|  |=====

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

    given GenCol[LocalDate] = Col.localDate(_)
    given GenCol[LocalTime] = Col.localTime(_)
    given GenCol[LocalDateTime] = Col.localDateTime(_)

    given GenCol[OffsetTime] = Col.offsetTime(_)
    given GenCol[OffsetDateTime] = Col.offsetDateTime(_)

  }

  sealed trait Constraint {

    override def toString: String =
      this match {
        case Constraint.PrimaryKey                                 => "PRIMARY KEY"
        case Constraint.ForeignKey(schemaName, tableName, colName) => s"REFERENCES $schemaName.$tableName($colName)"
      }

  }
  object Constraint {

    case object PrimaryKey extends Constraint

    final class ForeignKey private (val fkr: () => ForeignKeyRef) extends Constraint
    object ForeignKey {
      def apply(fkr: => ForeignKeyRef): ForeignKey = new ForeignKey(() => fkr)
      def unapply(foreignKey: ForeignKey): ForeignKeyRef = foreignKey.fkr()
    }

  }

}
