package harness.sql.errors

final case class ErrorWithSql(sql: String, cause: Throwable)
    extends Throwable(
      s"${Option(cause.getMessage).getOrElse(cause.toString)}\nsql: $sql",
    )
