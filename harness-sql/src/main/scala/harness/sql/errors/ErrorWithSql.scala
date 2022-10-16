package harness.sql.errors

import harness.core.*

final case class ErrorWithSql(sql: String, cause: HError)
    extends HError.Single(
      HError.UserMessage.hidden,
      s"${cause.internalMessage}\nsql: $sql",
      Nil,
    )
