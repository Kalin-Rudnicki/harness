package harness.sql.autoSchema

import harness.sql.Database
import zio.*

sealed trait MigrationEffect
object MigrationEffect {
  final case class Sql(sql: String) extends MigrationEffect
  final case class Code(name: String, code: RIO[Database, Unit]) extends MigrationEffect
}
