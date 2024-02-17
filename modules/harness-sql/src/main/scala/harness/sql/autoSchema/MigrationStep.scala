package harness.sql.autoSchema

import harness.sql.{Col, JDBCConnection}
import harness.zio.*
import zio.*
import zio.json.*
import zio.json.ast.*

object MigrationStep {

  sealed trait Encoded
  object Encoded {
    sealed trait SqlEncoded extends MigrationStep.Encoded {
      val sql: String
    }
    final case class Code(name: String, reversible: Boolean) extends MigrationStep.Encoded

    implicit val jsonCodec: JsonCodec[MigrationStep.Encoded] = {
      val derived = DeriveJsonCodec.gen[MigrationStep.Encoded]

      JsonCodec(
        derived.encoder,
        derived.decoder.orElse(
          Json.decoder.mapOrFail {
            case Json.Obj(fields) =>
              derived.decoder.decodeJson(
                Json
                  .Obj(
                    fields.map {
                      case ("SqlEncoded", Json.Obj(Chunk(tuple0))) => tuple0
                      case default                                 => default
                    },
                  )
                  .toJson,
              )
            case default => derived.decoder.decodeJson(default.toJson)
          },
        ),
      )
    }

  }

  sealed trait InMemory
  object InMemory {
    sealed trait Auto extends InMemory
    object Auto {
      implicit val ordering: Ordering[MigrationStep.InMemory.Auto] =
        Ordering.by[MigrationStep.InMemory.Auto, Int] {
          case DropCol(_, KeyType.NoKey)                 => 1
          case DropCol(_, _: KeyType.ForeignKey)         => 2
          case DropCol(_, KeyType.PrimaryKey)            => 3
          case _: DropTable                              => 4
          case _: DropSchema                             => 5
          case _: SetColNotNullable                      => 6
          case _: SetColNullable                         => 7
          case _: CreateSchema                           => 8
          case _: CreateTable                            => 9
          case CreateCol(_, _, KeyType.PrimaryKey, _)    => 10
          case CreateCol(_, _, _: KeyType.ForeignKey, _) => 11
          case CreateCol(_, _, KeyType.NoKey, _)         => 12
        }
    }

    final case class Code(name: String, up: RIO[HarnessEnv & JDBCConnection, Unit], down: Option[RIO[HarnessEnv & JDBCConnection, Unit]]) extends MigrationStep.InMemory
  }

  final case class CreateSchema(ref: SchemaRef.Custom) extends MigrationStep.Encoded.SqlEncoded with MigrationStep.InMemory.Auto {
    override val sql: String = s"CREATE SCHEMA $ref"
  }
  final case class RenameSchema(refBefore: SchemaRef.Custom, refAfter: SchemaRef.Custom) extends MigrationStep.Encoded.SqlEncoded with MigrationStep.InMemory {
    override val sql: String = s"ALTER SCHEMA $refBefore RENAME TO $refAfter"
  }
  final case class DropSchema(ref: SchemaRef.Custom) extends MigrationStep.Encoded.SqlEncoded with MigrationStep.InMemory.Auto {
    override val sql: String = s"DROP SCHEMA $ref"
  }

  final case class CreateTable(ref: TableRef) extends MigrationStep.Encoded.SqlEncoded with MigrationStep.InMemory.Auto {
    override val sql: String = s"CREATE TABLE ${ref.schemaRef}.${ref.tableName}()"
  }
  final case class RenameTable(schemaRef: SchemaRef, nameBefore: String, nameAfter: String) extends MigrationStep.Encoded.SqlEncoded with MigrationStep.InMemory {
    override val sql: String = s"ALTER TABLE $schemaRef.$nameBefore RENAME TO $nameAfter"
  }
  final case class DropTable(ref: TableRef) extends MigrationStep.Encoded.SqlEncoded with MigrationStep.InMemory.Auto {
    override val sql: String = s"DROP TABLE ${ref.schemaRef}.${ref.tableName}"
  }
  // TODO (KR) :  SetTableSchema

  final case class CreateCol(ref: ColRef, colType: Col.ColType, keyType: KeyType, nullable: Boolean) extends MigrationStep.Encoded.SqlEncoded with MigrationStep.InMemory.Auto {
    override val sql: String = {
      val nullableSql = if (nullable) "NULL" else "NOT NULL"
      s"ALTER TABLE ${ref.schemaRef}.${ref.tableName} ADD COLUMN ${ref.colName} $colType $nullableSql${keyType.sql}"
    }
  }
  final case class RenameCol(tableRef: TableRef, nameBefore: String, nameAfter: String) extends MigrationStep.Encoded.SqlEncoded with MigrationStep.InMemory {
    override val sql: String = s"ALTER TABLE ${tableRef.schemaRef}.${tableRef.tableName} RENAME COLUMN $nameBefore TO $nameAfter"
  }
  final case class DropCol(ref: ColRef, keyType: KeyType) extends MigrationStep.Encoded.SqlEncoded with MigrationStep.InMemory.Auto {
    override val sql: String = s"ALTER TABLE ${ref.schemaRef}.${ref.tableName} DROP COLUMN ${ref.colName}"
  }
  final case class SetColNotNullable(ref: ColRef) extends MigrationStep.Encoded.SqlEncoded with MigrationStep.InMemory.Auto {
    override val sql: String = s"ALTER TABLE ${ref.schemaRef}.${ref.tableName} ALTER COLUMN ${ref.colName} SET NOT NULL"
  }
  final case class SetColNullable(ref: ColRef) extends MigrationStep.Encoded.SqlEncoded with MigrationStep.InMemory.Auto {
    override val sql: String = s"ALTER TABLE ${ref.schemaRef}.${ref.tableName} ALTER COLUMN ${ref.colName} DROP NOT NULL"
  }

  final case class CreateIndex(tableRef: TableRef, name: String, unique: Boolean, cols: List[String]) extends MigrationStep.Encoded.SqlEncoded with MigrationStep.InMemory {
    override val sql: String = {
      val uniqueSql = if (unique) s" UNIQUE" else ""
      s"CREATE$uniqueSql INDEX $name ON ${tableRef.schemaRef}.${tableRef.tableName} (${cols.mkString(", ")})"
    }
  }
  final case class RenameIndex(nameBefore: String, nameAfter: String) extends MigrationStep.Encoded.SqlEncoded with MigrationStep.InMemory {
    override val sql: String = s"ALTER INDEX $nameBefore RENAME TO $nameAfter"
  }
  final case class DropIndex(schemaRef: SchemaRef, name: String) extends MigrationStep.Encoded.SqlEncoded with MigrationStep.InMemory {
    override val sql: String = s"DROP INDEX $schemaRef.$name"
  }

}
