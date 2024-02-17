package harness.sql.query

import harness.sql.JDBCConnection
import harness.sql.error.QueryError
import harness.zio.*
import zio.*

final class IntQueryResult private[query] (queryName: String, fragment: Fragment, effect: ZIO[JDBCConnection & Logger, QueryError, Int]) {

  def execute: ZIO[JDBCConnection & Logger & Telemetry, QueryError, Int] =
    effect.telemetrize("Executed SQL query", "query-name" -> queryName)

  inline def unit: ZIO[JDBCConnection & Logger & Telemetry, QueryError, Unit] = execute.unit

  inline private def withSizeCheck(
      inline check: Int => Boolean,
      inline expectedStr: => String,
  ): ZIO[JDBCConnection & Logger & Telemetry, QueryError, Unit] =
    execute.flatMap { actual =>
      if (check(actual)) ZIO.unit
      else ZIO.fail(QueryError(queryName, fragment.sql, QueryError.Cause.InvalidResultSetSize(expectedStr, actual)))
    }

  def expectSize(expected: Int): ZIO[JDBCConnection & Logger & Telemetry, QueryError, Unit] =
    withSizeCheck(_ == expected, expected.toString)

  def single: ZIO[JDBCConnection & Logger & Telemetry, QueryError, Unit] = expectSize(1)

  def option: ZIO[JDBCConnection & Logger & Telemetry, QueryError, Unit] = withSizeCheck(a => a == 0 || a == 1, "0..1")

}
