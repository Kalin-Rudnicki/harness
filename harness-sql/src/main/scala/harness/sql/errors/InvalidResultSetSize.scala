package harness.sql.errors

import harness.core.*

final case class InvalidResultSetSize(expected: String, actual: Int)
    extends HError.Single(
      HError.UserMessage.hidden,
      s"Invalid ResultSet size: Expected [$expected], but got ($actual)",
      Nil,
    )
