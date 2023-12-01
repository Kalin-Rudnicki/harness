package harness.sql.autoSchema

import cats.data.NonEmptyList
import harness.core.Version
import harness.sql.JDBCConnection
import harness.zio.*

final case class InMemoryMigration(version: Version, tables: Tables, steps: NonEmptyList[InMemoryMigration.Step])
object InMemoryMigration {

  def apply(version: Version, tables: Tables)(step0: InMemoryMigration.Step, stepN: InMemoryMigration.Step*): InMemoryMigration =
    new InMemoryMigration(version, tables, NonEmptyList(step0, stepN.toList))

  def auto(version: Version, tables: Tables, allowDrops: Boolean = false): InMemoryMigration =
    InMemoryMigration(version, tables)(InMemoryMigration.Step.Auto(tables, allowDrops))

  sealed trait Step
  object Step {
    final case class Auto(tables: Tables, allowDrops: Boolean = false) extends Step
    final case class Manual(step: MigrationStep.InMemory) extends Step

    def apply(step: MigrationStep.InMemory): InMemoryMigration.Step =
      InMemoryMigration.Step.Manual(step)
    def code(name: String)(up: SHRIO[JDBCConnection, Unit]): InMemoryMigration.Step =
      InMemoryMigration.Step.Manual(MigrationStep.InMemory.Code(name, up, None))
    def code(name: String)(up: SHRIO[JDBCConnection, Unit], down: SHRIO[JDBCConnection, Unit]): InMemoryMigration.Step =
      InMemoryMigration.Step.Manual(MigrationStep.InMemory.Code(name, up, Some(down)))
  }

}
