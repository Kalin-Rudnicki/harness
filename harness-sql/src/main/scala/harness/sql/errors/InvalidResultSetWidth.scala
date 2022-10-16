package harness.sql.errors

import harness.core.*

final case class InvalidResultSetWidth(expected: Int, actual: Int)
    extends HError.Single(
      HError.UserMessage.hidden,
      s"Invalid ResultSet width: Expected [$expected], but got ($actual)",
      Nil
    )
