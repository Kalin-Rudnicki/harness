package harness.sql

import cats.data.{EitherNel, NonEmptyList}
import cats.syntax.option.*
import harness.core.*
import harness.sql.typeclass.*
import java.time.*
import java.util.UUID
import scala.reflect.ClassTag
import zio.json.JsonCodec

final case class Col[T] private (
    colName: String,
    colType: String,
    colCodec: ColCodec[T],
    nullable: Boolean,
    constraints: List[Col.Constraint],
) {

  def imap[T2](mf: T => T2)(cmf: T2 => T): Col[T2] =
    Col(colName, colType, colCodec.imap(mf)(cmf), nullable, constraints)

  def iemap[T2](mf: T => EitherNel[String, T2])(cmf: T2 => T): Col[T2] =
    Col(colName, colType, colCodec.iemap(mf)(cmf), nullable, constraints)

  def optional: Col[Option[T]] =
    Col(colName, colType, colCodec.optional, true, constraints)

  def primaryKey: Col[T] =
    Col(colName, colType, colCodec, nullable, Col.Constraint.PrimaryKey :: constraints)
  def references(foreignKeyRef: => ForeignKeyRef): Col[T] =
    Col(colName, colType, colCodec, nullable, Col.Constraint.ForeignKey(foreignKeyRef) :: constraints)

  override def toString: String = s"$colName[$colType]"

}
object Col {

  def string(name: String): Col[String] = Col(name, "TEXT", ColCodec.string, false, Nil)
  def uuid(name: String): Col[UUID] = Col(name, "UUID", ColCodec.uuid, false, Nil)
  def boolean(name: String): Col[Boolean] = Col(name, "BOOLEAN", ColCodec.boolean, false, Nil)

  def short(name: String): Col[Short] = Col(name, "SMALLINT", ColCodec.short, false, Nil)
  def int(name: String): Col[Int] = Col(name, "INTEGER", ColCodec.int, false, Nil)
  def long(name: String): Col[Long] = Col(name, "BIGINT", ColCodec.long, false, Nil)

  def float(name: String): Col[Float] = Col(name, "REAL", ColCodec.float, false, Nil)
  def double(name: String): Col[Double] = Col(name, "DOUBLE PRECISION", ColCodec.double, false, Nil)

  def date(name: String): Col[LocalDate] = Col(name, "DATE", ColCodec.date, false, Nil)
  def time(name: String): Col[LocalTime] = Col(name, "TIME", ColCodec.time, false, Nil)
  def dateTime(name: String): Col[LocalDateTime] = Col(name, "TIMESTAMP", ColCodec.dateTime, false, Nil)

  def json[T: JsonCodec](name: String): Col[T] = Col(name, "JSON", ColCodec.json[T], false, Nil)
  def jsonb[T: JsonCodec](name: String): Col[T] = Col(name, "JSONB", ColCodec.json[T], false, Nil)

  def `enum`[E <: Enum[E], Enc](colName: String)(implicit ewe: Enum.WithEnc[E, Enc], gc: Col.GenCol[Enc], ct: ClassTag[E]): Col[E] =
    gc.make(colName).iemap { v => ewe.decode(v).toRight(NonEmptyList.one(s"Invalid ${ct.runtimeClass.getSimpleName}: $v")) }(ewe.encode)

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
