package harness.sql.error

import harness.core.*

sealed trait MigrationError extends Throwable {
  override final def getMessage: String = this match {
    case MigrationError.ConnectionError(error) =>
      s"Error getting connection for migration: ${error.safeGetMessage}"
    case MigrationError.QueryError(error) =>
      s"Error executing migration query: ${error.safeGetMessage}"
    case MigrationError.Invalid(message) =>
      s"Invalid input provided to migration: $message"
    case MigrationError.UnableToExecuteCodeStep(cause) =>
      s"Error when executing migration manual code step: ${cause.safeGetMessage}"
  }
}
object MigrationError {
  final case class ConnectionError(error: harness.sql.error.ConnectionError) extends MigrationError
  final case class QueryError(error: harness.sql.error.QueryError) extends MigrationError
  final case class Invalid(message: String) extends MigrationError
  final case class UnableToExecuteCodeStep(cause: Throwable) extends MigrationError
}
