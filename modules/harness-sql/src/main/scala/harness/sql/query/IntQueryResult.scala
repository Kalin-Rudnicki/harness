package harness.sql.query

import harness.sql.Database
import harness.sql.error.QueryError
import harness.zio.*
import zio.*

final class IntQueryResult private[query] (queryName: String, fragment: Fragment, effect: ZIO[Database, QueryError, Int]) {

  def execute: ZIO[Database, QueryError, Int] =
    effect.telemetrize("Executed SQL query", "query-name" -> queryName)

  inline def unit: ZIO[Database, QueryError, Unit] = execute.unit

  inline private def withSizeCheck(
      inline check: Int => Boolean,
      inline expectedStr: => String,
  ): ZIO[Database, QueryError, Unit] =
    execute.flatMap { actual =>
      ZIO.fail(QueryError(queryName, fragment.sql, QueryError.Cause.InvalidResultSetSize(expectedStr, actual))).unlessDiscard(check(actual))
    }

  def expectSize(expected: Int): ZIO[Database, QueryError, Unit] =
    withSizeCheck(_ == expected, expected.toString)

  def single: ZIO[Database, QueryError, Unit] = expectSize(1)

  def option: ZIO[Database, QueryError, Unit] = withSizeCheck(a => a == 0 || a == 1, "0..1")

}
