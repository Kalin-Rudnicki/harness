package harness.sql.mock.error

sealed trait MockError extends Throwable {
  override final def getMessage: String = this match {
    case MockError.ConstraintViolationError(table, index, key) =>
      s"Constraint violation for table $table on index $index, key: $key"
    case MockError.MissingExpectedKeyError(table, key) =>
      s"Expected single value in table $table for key: $key"
  }
}
object MockError {
  final case class ConstraintViolationError(table: String, index: String, key: Any) extends MockError
  final case class MissingExpectedKeyError(table: String, key: Any) extends MockError
}
