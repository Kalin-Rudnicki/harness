package harness.sql.error

sealed trait MigrationError extends Throwable
object MigrationError {
  final case class ConnectionError(error: harness.sql.error.ConnectionError) extends MigrationError
  final case class QueryError(error: harness.sql.error.QueryError) extends MigrationError
  final case class Invalid(message: String) extends MigrationError
  final case class UnableToExecuteCodeStep(cause: Throwable) extends MigrationError
}
