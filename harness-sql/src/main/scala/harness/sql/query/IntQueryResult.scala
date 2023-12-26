package harness.sql.query

import harness.core.HError
import harness.sql.JDBCConnection
import harness.sql.errors.*
import harness.zio.*
import zio.*

final class IntQueryResult private[query] (queryName: String, fragment: Fragment, effect: HRIO[JDBCConnection & Logger, Int]) {

  def execute: HRIO[JDBCConnection & Logger & Telemetry, Int] =
    effect.mapError(ErrorWithSql(fragment.sql, _)).telemetrize("Executed SQL query", "query-name" -> queryName)

  inline def unit: HRIO[JDBCConnection & Logger & Telemetry, Unit] = execute.unit

  inline private def withSizeCheck(
      inline check: Int => Boolean,
      inline expectedStr: => String,
      inline userMessage: => HError.UserMessage,
  ): HRIO[JDBCConnection & Logger & Telemetry, Unit] =
    execute.flatMap { actual =>
      if (check(actual)) ZIO.unit
      else ZIO.fail(ErrorWithSql(fragment.sql, InvalidResultSetSize(queryName, userMessage, expectedStr, actual)))
    }

  inline private def withSizeCheckOr[E](
      inline check: Int => Boolean,
      inline expectedStr: => String,
      inline error: => E,
  ): HZIO[JDBCConnection & Logger & Telemetry, E, Unit] =
    withSizeCheck(check, expectedStr, HError.UserMessage.hidden).mapError(HError.Or(error, _))

  def expectSize(expected: Int): HRIO[JDBCConnection & Logger & Telemetry, Unit] =
    withSizeCheck(_ == expected, expected.toString, HError.UserMessage.hidden)
  def expectSize(expected: Int, userMessage: => String): HRIO[JDBCConnection & Logger & Telemetry, Unit] =
    withSizeCheck(_ == expected, expected.toString, HError.UserMessage.Const(userMessage))
  def expectSizeOr[E](expected: Int, error: => E): HZIO[JDBCConnection & Logger & Telemetry, E, Unit] =
    withSizeCheckOr(_ == expected, expected.toString, error)

  inline def single: HRIO[JDBCConnection & Logger & Telemetry, Unit] = expectSize(1)
  inline def single(userMessage: => String): HRIO[JDBCConnection & Logger & Telemetry, Unit] = expectSize(1, userMessage)
  inline def singleOr[E](error: => E): HZIO[JDBCConnection & Logger & Telemetry, E, Unit] = expectSizeOr(1, error)

  def option: HRIO[JDBCConnection & Logger & Telemetry, Unit] =
    withSizeCheck(a => a == 0 || a == 1, "0..1", HError.UserMessage.hidden)

}
