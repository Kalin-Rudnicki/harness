package harness.sql.errors

final case class InvalidResultSetWidth(expected: Int, actual: Int)
    extends Throwable(
      s"Invalid ResultSet width: Expected [$expected], but got ($actual)",
    )
