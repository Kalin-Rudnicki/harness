package harness.sql.errors

import harness.core.*

final case class InvalidResultSetSize(queryName: String, _userMessage: HError.UserMessage, expected: String, actual: Int)
    extends HError.Single(
      _userMessage,
      s"Invalid ResultSet size for query '$queryName': Expected [$expected], but got ($actual)",
      Nil,
    )
