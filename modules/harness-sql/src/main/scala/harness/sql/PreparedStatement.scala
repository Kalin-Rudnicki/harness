package harness.sql

import harness.sql.error.QueryError
import harness.sql.query.*
import harness.sql.typeclass.*
import harness.zio.*
import zio.*

final class PreparedStatement private (
    queryName: String,
    fragment: Fragment,
    ps: java.sql.PreparedStatement,
) {

  private inline def attemptGeneric[A](hint: String)(thunk: => A): IO[QueryError, A] =
    QueryError.attempt(queryName, fragment.sql) { thunk }(QueryError.Cause.Generic(hint, _))

  def writeSingle[I, V](
      i: I,
      input: Input[I, V],
  ): IO[QueryError, Unit] =
    for {
      unmappedInputs <- attemptGeneric("Unable to create unmapped inputs") { input.buildUnmappedInputs(i) }
      encodedInputs <- attemptGeneric("Unable to encode inputs") { fragment.qim.prepare(unmappedInputs) }
      _ <- attemptGeneric("Unable to write inputs to prepared statement") {
        encodedInputs.zipWithIndexFrom(1).foreach { case (value, i) =>
          ps.setObject(i, value.value)
        }
      }
      // TODO (KR) : log?
    } yield ()

  def writeBatched[I](
      is: Chunk[I],
      input: Input[I, ?],
  ): IO[QueryError, Unit] =
    ZIO.foreachDiscard(is) { i =>
      writeSingle(i, input) *>
        attemptGeneric("Unable to add batch") { ps.addBatch(); ps.clearParameters() }
    }

  def executeQuery[O](decoder: QueryDecoderMany[O]): ZIO[Scope, QueryError, ResultSet[O]] =
    QueryError
      .attempt(queryName, fragment.sql) { ps.executeQuery() }(QueryError.Cause.UnableToExecuteQuery(_))
      .autoClose
      .map(new ResultSet(queryName, fragment, _, decoder))

  def executeUpdate: IO[QueryError, Int] =
    QueryError.attempt(queryName, fragment.sql) { ps.executeUpdate() }(QueryError.Cause.UnableToExecuteQuery(_))

  def executeBatch: IO[QueryError, Chunk[Int]] =
    QueryError.attempt(queryName, fragment.sql) { Chunk.fromArray { ps.executeBatch() } }(QueryError.Cause.UnableToExecuteQuery(_))

}
object PreparedStatement {

  def make(queryName: String, fragment: Fragment): ZIO[Database & Scope, QueryError, PreparedStatement] =
    for {
      con <- Database.getConnection.mapError(e => QueryError(queryName, fragment.sql, QueryError.Cause.ErrorGettingConnection(e)))
      ps <- QueryError.attempt(queryName, fragment.sql) { con.jdbcConnection.prepareStatement(fragment.sql) }(QueryError.Cause.Generic("Unable to create prepared statement", _)).autoClose
    } yield PreparedStatement(queryName, fragment, ps)

}
