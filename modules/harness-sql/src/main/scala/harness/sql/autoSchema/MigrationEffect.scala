package harness.sql.autoSchema

import harness.sql.JDBCConnection
import harness.zio.*
import zio.*

sealed trait MigrationEffect
object MigrationEffect {
  final case class Sql(sql: String) extends MigrationEffect
  final case class Code(name: String, code: RIO[HarnessEnv & JDBCConnection, Unit]) extends MigrationEffect
}
