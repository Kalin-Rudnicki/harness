package harness.sql.autoSchema

import harness.sql.{Col, JDBCConnection}
import harness.zio.*
import zio.json.*

object MigrationStep {

  sealed trait Encoded
  object Encoded {
    final case class Code(name: String, reversible: Boolean) extends MigrationStep.Encoded

    implicit val jsonCodec: JsonCodec[MigrationStep.Encoded] = DeriveJsonCodec.gen
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

    final case class Code(name: String, up: SHRIO[JDBCConnection, Unit], down: Option[SHRIO[JDBCConnection, Unit]]) extends MigrationStep.InMemory
  }

  final case class CreateSchema(ref: SchemaRef.Custom) extends MigrationStep.Encoded with MigrationStep.InMemory.Auto
  final case class RenameSchema(refBefore: SchemaRef.Custom, refAfter: SchemaRef.Custom) extends MigrationStep.Encoded with MigrationStep.InMemory
  final case class DropSchema(ref: SchemaRef.Custom) extends MigrationStep.Encoded with MigrationStep.InMemory.Auto

  final case class CreateTable(ref: TableRef) extends MigrationStep.Encoded with MigrationStep.InMemory.Auto
  final case class RenameTable(schemaRef: SchemaRef, nameBefore: String, nameAfter: String) extends MigrationStep.Encoded with MigrationStep.InMemory
  final case class DropTable(ref: TableRef) extends MigrationStep.Encoded with MigrationStep.InMemory.Auto
  // TODO (KR) :  SetTableSchema

  final case class CreateCol(ref: ColRef, colType: Col.ColType, keyType: KeyType, nullable: Boolean) extends MigrationStep.Encoded with MigrationStep.InMemory.Auto
  final case class RenameCol(tableRef: TableRef, nameBefore: String, nameAfter: String) extends MigrationStep.Encoded with MigrationStep.InMemory
  final case class DropCol(ref: ColRef, keyType: KeyType) extends MigrationStep.Encoded with MigrationStep.InMemory.Auto
  final case class SetColNotNullable(ref: ColRef) extends MigrationStep.Encoded with MigrationStep.InMemory.Auto
  final case class SetColNullable(ref: ColRef) extends MigrationStep.Encoded with MigrationStep.InMemory.Auto

  final case class CreateIndex(tableRef: TableRef, name: String, unique: Boolean, cols: List[String]) extends MigrationStep.Encoded with MigrationStep.InMemory
  final case class RenameIndex(nameBefore: String, nameAfter: String) extends MigrationStep.Encoded with MigrationStep.InMemory
  final case class DropIndex(name: String) extends MigrationStep.Encoded with MigrationStep.InMemory

}
