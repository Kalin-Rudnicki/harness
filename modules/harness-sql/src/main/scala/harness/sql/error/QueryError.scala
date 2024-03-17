package harness.sql.error

import cats.data.NonEmptyList
import harness.core.*
import zio.*

final case class QueryError(
    queryName: String,
    sql: String,
    cause: QueryError.Cause,
) extends Throwable {
  override def getMessage: String = s"Error executing query '$queryName'\nsql: $sql\ncause: $cause"
}
object QueryError {

  sealed trait Cause {

    override final def toString: String = this match {
      case Cause.InvalidResultSetSize(expected, actual) =>
        s"Query result had unexpected size. Expected = $expected, actual = $actual."
      case Cause.RowDecodeFailure(errors, values) =>
        errors.toList match {
          case error :: Nil =>
            s"""
               |  Error decoding query result: $error
               |  Result values:${values.map(v => s"\n    - $v").mkString}""".stripMargin
          case errors =>
            s"""
               |  Errors decoding query result:${errors.map(e => s"\n    - $e").mkString}
               |  Result values:${values.map(v => s"\n    - $v").mkString}""".stripMargin
        }
      case Cause.InvalidResultSetWidth(expected, actual) =>
        s"Query result had unexpected number of columns. Expected = $expected, actual = $actual."
      case Cause.UnableToExecuteQuery(cause) =>
        s"Error executing query: ${cause.safeGetMessage}"
      case Cause.Generic(message, cause) =>
        s"Generic query error '$message', cause: ${cause.safeGetMessage}"
    }

  }
  object Cause {

    final case class InvalidResultSetSize(expected: String, actual: Int) extends Cause
    final case class RowDecodeFailure(errors: NonEmptyList[String], values: IArray[Object]) extends Cause
    final case class InvalidResultSetWidth(expected: Int, actual: Int) extends Cause
    final case class UnableToExecuteQuery(cause: Throwable) extends Cause

    // TODO (KR) : try to eliminate this
    final case class Generic(message: String, cause: Throwable) extends Cause

    // =====|  |=====

    // TODO (KR) : pull out specific errors
    def fromThrowable(message: String, throwable: Throwable): Cause =
      Cause.Generic(message, throwable)

  }

  private[sql] def attempt[A](queryName: String, sql: String)(thunk: => A)(mapError: Throwable => QueryError.Cause): IO[QueryError, A] =
    ZIO.attempt(thunk).mapError(err => QueryError(queryName, sql, mapError(err)))

}
