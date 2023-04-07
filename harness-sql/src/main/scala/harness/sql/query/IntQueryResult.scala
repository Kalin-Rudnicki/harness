package harness.sql.query

import harness.core.HError
import harness.sql.JDBCConnection
import harness.sql.errors.*
import harness.zio.*
import zio.*

final class IntQueryResult private[query] (queryName: String, fragment: Fragment, effect: HRIO[JDBCConnection & Logger, Int]) {

  def execute: HRIO[JDBCConnection & Logger & Telemetry, Int] =
    effect.mapError(ErrorWithSql(fragment.sql, _)).trace("Executed SQL query", "query-name" -> queryName)

  inline def unit: HRIO[JDBCConnection & Logger & Telemetry, Unit] = execute.unit
  
  inline private def withSizeCheck(
      inline check: Int => Boolean,
      inline expectedStr: String,
      inline userMessage: HError.UserMessage,
  ): HRIO[JDBCConnection & Logger & Telemetry, Unit] =
    execute.flatMap { actual =>
      if (check(actual)) ZIO.unit
      else ZIO.fail(ErrorWithSql(fragment.sql, InvalidResultSetSize(queryName, userMessage, expectedStr, actual)))
    }

  def expectSize(expected: Int): HRIO[JDBCConnection & Logger & Telemetry, Unit] =
    withSizeCheck(_ == expected, expected.toString, HError.UserMessage.hidden)
  def expectSize(expected: Int, userMessage: String): HRIO[JDBCConnection & Logger & Telemetry, Unit] =
    withSizeCheck(_ == expected, expected.toString, HError.UserMessage.Const(userMessage))

  inline def single: HRIO[JDBCConnection & Logger & Telemetry, Unit] = expectSize(1)
  inline def single(userMessage: String): HRIO[JDBCConnection & Logger & Telemetry, Unit] = expectSize(1, userMessage)

  def option: HRIO[JDBCConnection & Logger & Telemetry, Unit] =
    withSizeCheck(a => a == 0 || a == 1, "0..1", HError.UserMessage.hidden)

}
