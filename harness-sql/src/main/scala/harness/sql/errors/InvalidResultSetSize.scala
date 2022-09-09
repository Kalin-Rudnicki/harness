package harness.sql.errors

final case class InvalidResultSetSize(expected: String, actual: Int) extends Throwable(
  s"Invalid ResultSet size: Expected [$expected], but got ($actual)"
)
