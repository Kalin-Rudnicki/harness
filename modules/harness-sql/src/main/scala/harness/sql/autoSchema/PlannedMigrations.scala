package harness.sql.autoSchema

import cats.data.NonEmptyList

final case class PlannedMigrations(migrations: NonEmptyList[InMemoryMigration])
object PlannedMigrations {

  def apply(migration0: InMemoryMigration, migrationN: InMemoryMigration*): PlannedMigrations =
    PlannedMigrations(NonEmptyList(migration0, migrationN.toList))

}
