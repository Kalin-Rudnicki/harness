package harness.sql.mock.error

sealed trait MockError extends Throwable {
  // TODO (KR) : getMessage
}
object MockError {
  final case class ConstraintViolationError(table: String, index: String, key: Any) extends MockError
  final case class MissingExpectedKeyError(table: String, key: Any) extends MockError
}
