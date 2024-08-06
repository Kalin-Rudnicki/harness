package harness.sql.autoSchema

import cats.data.NonEmptyList
import harness.core.Version
import harness.sql.Database
import zio.*

final case class InMemoryMigration(version: Version, tables: Tables, steps: NonEmptyList[InMemoryMigration.Step])
object InMemoryMigration {

  type StepType = InMemoryMigration.Step | InMemoryMigration.StepType.Auto | MigrationStep.InMemory
  object StepType {

    final case class Auto(allowDrops: Boolean = false)

    def toStep(stepType: StepType, tables: Tables): InMemoryMigration.Step =
      stepType match {
        case step: InMemoryMigration.Step          => step
        case auto: InMemoryMigration.StepType.Auto => InMemoryMigration.Step.Auto(tables, auto.allowDrops)
        case inMemory: MigrationStep.InMemory      => InMemoryMigration.Step.Manual(inMemory)
      }

  }

  def apply(version: Version, tables: Tables)(step0: InMemoryMigration.StepType, stepN: InMemoryMigration.StepType*): InMemoryMigration =
    new InMemoryMigration(version, tables, NonEmptyList(step0, stepN.toList).map(StepType.toStep(_, tables)))

  def auto(version: Version, tables: Tables, allowDrops: Boolean = false): InMemoryMigration =
    InMemoryMigration(version, tables)(InMemoryMigration.Step.Auto(tables, allowDrops))

  sealed trait Step
  object Step {
    final case class Auto(tables: Tables, allowDrops: Boolean = false) extends Step
    final case class Manual(step: MigrationStep.InMemory) extends Step

    def apply(step: MigrationStep.InMemory): InMemoryMigration.Step =
      InMemoryMigration.Step.Manual(step)
    def code(name: String)(up: RIO[Database, Unit]): InMemoryMigration.Step =
      InMemoryMigration.Step.Manual(MigrationStep.InMemory.Code(name, up, None))
    def code(name: String)(up: RIO[Database, Unit], down: RIO[Database, Unit]): InMemoryMigration.Step =
      InMemoryMigration.Step.Manual(MigrationStep.InMemory.Code(name, up, Some(down)))
  }

}
