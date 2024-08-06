package harness.sql.query

import harness.sql.Database
import harness.sql.error.QueryError
import harness.zio.*
import zio.*

final class BatchQueryResult private[query] (queryName: String, @scala.annotation.unused fragment: Fragment, effect: ZIO[Database, QueryError, Chunk[Int]]) {

  def execute: ZIO[Database, QueryError, Chunk[Int]] =
    effect.telemetrize("Executed SQL query", "query-name" -> queryName)

  inline def unit: ZIO[Database, QueryError, Unit] = execute.unit

  // TODO (KR) : remove or use
  /*
  inline private def withSizeCheck(
      inline check: Chunk[Int] => Boolean,
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
   */

}
