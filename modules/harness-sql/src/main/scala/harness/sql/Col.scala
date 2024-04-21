package harness.sql

import cats.data.{EitherNel, NonEmptyList}
import cats.syntax.either.*
import cats.syntax.option.*
import harness.core.*
import harness.sql.typeclass.*
import harness.zio.*
import harness.zio.json.*
import java.time.*
import java.time.format.DateTimeFormatter
import java.util.{TimeZone, UUID}
import scala.reflect.ClassTag
import scala.util.Try
import zio.json.JsonCodec

final case class Col[T] private (
    colName: String,
    colType: Col.ColType,
    colCodec: ColCodec[T],
    nullable: Boolean,
    keyType: Option[Col.KeyType],
    setType: Option[String],
    getType: Option[String],
) { self =>

  def imap[T2](mf: T => T2)(cmf: T2 => T): Col[T2] =
    Col(colName, colType, colCodec.imap(mf)(cmf), nullable, keyType, setType, getType)
  def imapAuto[T2](implicit iMap: IMap[T, T2]): Col[T2] =
    self.imap(iMap.to)(iMap.from)

  def iemap[T2](mf: T => EitherNel[String, T2])(cmf: T2 => T): Col[T2] =
    Col(colName, colType, colCodec.iemap(mf)(cmf), nullable, keyType, setType, getType)
  def iemapAuto[T2](implicit iEMap: IEMap[T, T2]): Col[T2] =
    self.iemap(iEMap.toOrFail)(iEMap.from)

  inline def imapTry[T2](mf: T => T2)(cmf: T2 => T): Col[T2] =
    iemap(t => Try(mf(t)).toEither.leftMap(e => NonEmptyList.one(e.toString)))(cmf)

  def optional: Col[Option[T]] =
    Col(colName, colType, colCodec.optional, true, keyType, setType, getType)

  def primaryKey: Col[T] =
    Col(colName, colType, colCodec, nullable, Col.KeyType.PrimaryKey.some, setType, getType)
  def references(foreignKeyRef: => ForeignKeyRef): Col[T] =
    Col(colName, colType, colCodec, nullable, Col.KeyType.ForeignKey(foreignKeyRef).some, setType, getType)

  override def toString: String = s"$colName[$colType]"

  def ? : String = setType.fold("?")(t => s"? :: $t")
  def `(?)` : String = setType.fold("?")(t => s"(? :: $t)")

}
object Col {

  // =====| BuiltIn |=====

  private inline def basic[T](colName: String, colType: ColType, colCodec: ColCodec[T]): Col[T] = Col(colName, colType, colCodec, false, None, None, None)

  def string(name: String): Col[String] = Col.basic(name, ColType.Text, ColCodec.string)
  def uuid(name: String): Col[UUID] = Col.basic(name, ColType.UUID, ColCodec.uuid)
  def boolean(name: String): Col[Boolean] = Col.basic(name, ColType.Boolean, ColCodec.boolean)

  def short(name: String): Col[Short] = Col.basic(name, ColType.SmallInt, ColCodec.short)
  def int(name: String): Col[Int] = Col.basic(name, ColType.Integer, ColCodec.int)
  def long(name: String): Col[Long] = Col.basic(name, ColType.BigInt, ColCodec.long)

  def float(name: String): Col[Float] = Col.basic(name, ColType.Real, ColCodec.float)
  def double(name: String): Col[Double] = Col.basic(name, ColType.DoublePrecision, ColCodec.double)

  def localDate(name: String): Col[LocalDate] = Col.basic(name, ColType.Date, ColCodec.localDate)
  def localTime(name: String): Col[LocalTime] = Col.basic(name, ColType.Time, ColCodec.localTime)
  def localDateTime(name: String): Col[LocalDateTime] = Col.basic(name, ColType.Timestamp, ColCodec.localDateTime)

  def json[T: JsonCodec](name: String): Col[T] = Col(name, ColType.Json, ColCodec.json[T], false, None, "JSON".some, None)
  def jsonb[T: JsonCodec](name: String): Col[T] = Col(name, ColType.JsonB, ColCodec.json[T], false, None, "JSONB".some, None)

  // =====| Helpers |=====

  def mappedEnum[E <: Enum[E], Enc](colName: String)(implicit ewe: Enum.WithEnc[E, Enc], gc: Col.GenCol[Enc], ct: ClassTag[E]): Col[E] =
    gc.make(colName).iemap { v => ewe.decode(v).toRight(NonEmptyList.one(s"Invalid ${ct.runtimeClass.getSimpleName}: $v")) }(ewe.encode)

  def encoded[T](colName: String)(implicit encoder: StringEncoder[T], decoder: StringDecoder[T]): Col[T] =
    Col.string(colName).iemap(decoder.decodeAccumulating)(encoder.encode)

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

  enum ColType(final val sql: String) extends Enum[ColType] {
    case Text extends ColType("TEXT")
    case UUID extends ColType("UUID")
    case Boolean extends ColType("BOOLEAN")
    case SmallInt extends ColType("SMALLINT")
    case Integer extends ColType("INTEGER")
    case BigInt extends ColType("BIGINT")
    case Real extends ColType("REAL")
    case DoublePrecision extends ColType("DOUBLE PRECISION")
    case Date extends ColType("DATE")
    case Time extends ColType("TIME")
    case Timestamp extends ColType("TIMESTAMP")
    case Json extends ColType("JSON")
    case JsonB extends ColType("JSONB")

    override final def toString: String = sql
  }
  object ColType extends Enum.Companion[ColType] {
    implicit val jsonCodec: JsonCodec[ColType] = JsonCodec.`enum`[ColType, String]
  }

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

  sealed trait KeyType {

    final def sql: String =
      this match {
        case KeyType.PrimaryKey                                 => "PRIMARY KEY"
        case KeyType.ForeignKey(schemaName, tableName, colName) => s"REFERENCES $schemaName.$tableName($colName)"
      }

  }
  object KeyType {
    case object PrimaryKey extends KeyType
    final class ForeignKey(val fkr: () => ForeignKeyRef) extends KeyType {
      override def equals(obj: Any): Boolean =
        obj.asInstanceOf[Matchable] match {
          case that: ForeignKey => this.fkr() == that.fkr()
          case _                => false
        }
    }
    object ForeignKey {
      def apply(fkr: => ForeignKeyRef): ForeignKey = new ForeignKey(() => fkr)
      def unapply(foreignKey: ForeignKey): ForeignKeyRef = foreignKey.fkr()
    }
  }

}
