package harness.sql.error

import cats.data.NonEmptyList
import zio.*

final case class QueryError(
    queryName: String,
    sql: String,
    cause: QueryError.Cause,
) extends Throwable
object QueryError {

  sealed trait Cause extends Throwable
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
