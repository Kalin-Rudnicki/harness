package harness.sql.errors

import harness.core.*

final case class ErrorWithSql(sql: String, cause: HError)
    extends HError.Single(
      cause.userMessage,
      s"${cause.internalMessage}\nsql: $sql",
      cause :: Nil,
    )
