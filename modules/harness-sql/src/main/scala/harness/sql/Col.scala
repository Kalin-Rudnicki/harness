package harness.sql

import cats.data.{EitherNel, NonEmptyList}
import cats.syntax.either.*
import cats.syntax.option.*
import harness.core.*
import harness.sql.typeclass.*
import harness.zio.json.*
import java.time.*
import java.util.{TimeZone, UUID}
import scala.reflect.ClassTag
import scala.util.Try
import zio.json.*

final case class Col[T](
    colName: String,
    colType: Col.ColType,
    codec: QueryCodecSingle[T],
    nullable: Boolean,
    keyType: Option[Col.KeyType],
) { self =>

  def imap[T2](mf: T => T2)(cmf: T2 => T): Col[T2] =
    Col(colName, colType, codec.imap(mf)(cmf), nullable, keyType)
  def imapAuto[T2](implicit iMap: IMap[T, T2]): Col[T2] =
    self.imap(iMap.to)(iMap.from)

  def iemap[T2](mf: T => EitherNel[String, T2])(cmf: T2 => T): Col[T2] =
    Col(colName, colType, codec.iemap(mf)(cmf), nullable, keyType)
  def iemapAuto[T2](implicit iEMap: IEMap[T, T2]): Col[T2] =
    self.iemap(iEMap.toOrFail)(iEMap.from)

  inline def imapTry[T2](mf: T => T2)(cmf: T2 => T): Col[T2] =
    self.iemap(t => Try(mf(t)).toEither.leftMap(e => NonEmptyList.one(e.safeGetMessage)))(cmf)

  def optional: Col[Option[T]] =
    Col(colName, colType, codec.optional, true, keyType)

  def primaryKey: Col[T] =
    Col(colName, colType, codec, nullable, Col.KeyType.PrimaryKey.some)
  def references(foreignKeyRef: => ForeignKeyRef): Col[T] =
    Col(colName, colType, codec, nullable, Col.KeyType.ForeignKey(foreignKeyRef).some)

  def klass: Option[Class[?]] = codec.encoder.klass

  override def toString: String = s"$colName[$colType]"

}
object Col {

  // =====| Instances |=====

  private def basic[T](colName: String, colType: ColType, codec: QueryCodecSingle[T]): Col[T] = Col(colName, colType, codec, false, None)

  // --- Built In ---

  def string(name: String): Col[String] = Col.basic(name, ColType.Text, QueryCodecSingle.string)
  def uuid(name: String): Col[UUID] = Col.basic(name, ColType.UUID, QueryCodecSingle.uuid)
  def boolean(name: String): Col[Boolean] = Col.basic(name, ColType.Boolean, QueryCodecSingle.boolean)

  def short(name: String): Col[Short] = Col.basic(name, ColType.SmallInt, QueryCodecSingle.short)
  def int(name: String): Col[Int] = Col.basic(name, ColType.Integer, QueryCodecSingle.int)
  def long(name: String): Col[Long] = Col.basic(name, ColType.BigInt, QueryCodecSingle.long)

  def float(name: String): Col[Float] = Col.basic(name, ColType.Real, QueryCodecSingle.float)
  def double(name: String): Col[Double] = Col.basic(name, ColType.DoublePrecision, QueryCodecSingle.double)

  def localDate(name: String): Col[LocalDate] = Col.basic(name, ColType.Date, QueryCodecSingle.localDate)
  def localTime(name: String): Col[LocalTime] = Col.basic(name, ColType.Time, QueryCodecSingle.localTime)
  def localDateTime(name: String): Col[LocalDateTime] = Col.basic(name, ColType.Timestamp, QueryCodecSingle.localDateTime)

  def offsetDateTime(name: String): Col[OffsetDateTime] = Col.basic(name, ColType.TimestampWithZone, QueryCodecSingle.offsetDateTime)

  def json(name: String): Col[String] = Col.basic(name, ColType.Json, QueryCodecSingle.json)
  def jsonb(name: String): Col[String] = Col.basic(name, ColType.JsonB, QueryCodecSingle.json)
  def encodedJson[T: JsonCodec](name: String): Col[T] = Col.basic(name, ColType.Json, QueryCodecSingle.encodedJson[T])
  def encodedJsonb[T: JsonCodec](name: String): Col[T] = Col.basic(name, ColType.JsonB, QueryCodecSingle.encodedJsonb[T])

  // --- Mapped ---

  def `enum`[E <: Enum[E]](colName: String)(implicit ewe: Enum.WithEnc[E, String], ct: ClassTag[E]): Col[E] =
    Col.string(colName).iemap { v => ewe.decode(v).toRight(NonEmptyList.one(s"Invalid ${ct.runtimeClass.getSimpleName}: $v")) }(ewe.encode)

  def encoded[T](colName: String)(implicit encoder: StringEncoder[T], decoder: StringDecoder[T]): Col[T] =
    Col.string(colName).iemap(decoder.decodeAccumulating)(encoder.encode)

  def instant(name: String): Col[Instant] =
    Col.offsetDateTime(name).imap(_.toInstant)(_.atOffset(ZoneOffset.UTC))
  def zonedDateTime(name: String): Col[ZonedDateTime] =
    Col.offsetDateTime(name).imap(_.toZonedDateTime)(_.toOffsetDateTime)

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
    case TimestampWithZone extends ColType("TIMESTAMPTZ")
    case Json extends ColType("JSON")
    case JsonB extends ColType("JSONB")

    override final def toString: String = sql
  }
  object ColType extends Enum.Companion[ColType] {
    implicit val jsonCodec: JsonCodec[ColType] = JsonCodec.`enum`[ColType, String]
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
