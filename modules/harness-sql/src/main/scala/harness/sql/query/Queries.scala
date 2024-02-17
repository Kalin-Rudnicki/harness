package harness.sql.query

import cats.data.EitherNel
import cats.syntax.option.*
import harness.sql.*
import harness.sql.error.QueryError
import harness.sql.typeclass.*
import zio.*

final case class Query(queryName: String, fragment: Fragment) {
  def apply(): IntQueryResult =
    IntQueryResult(
      queryName,
      fragment,
      ZIO.scoped {
        Utils
          .preparedStatement(queryName, fragment, None)
          .flatMap { ps => QueryError.attempt(queryName, fragment.sql) { ps.executeUpdate() }(QueryError.Cause.UnableToExecuteQuery(_)) }
      },
    )
}

final case class QueryI[I](queryName: String, fragment: Fragment, encoder: RowEncoder[I]) {
  def apply(i: I): IntQueryResult =
    IntQueryResult(
      queryName,
      fragment,
      ZIO.scoped {
        Utils.preparedStatement(queryName, fragment, (i, encoder).some).flatMap { ps =>
          QueryError.attempt(queryName, fragment.sql) { ps.executeUpdate() }(QueryError.Cause.UnableToExecuteQuery(_))
        }
      },
    )
  def batched(is: Chunk[I]): IntQueryResult =
    IntQueryResult(
      queryName,
      fragment,
      ZIO.scoped {
        Utils.batchPreparedStatement(queryName, fragment, encoder, is).flatMap { ps =>
          QueryError.attempt(queryName, fragment.sql) { ps.executeBatch() }(QueryError.Cause.UnableToExecuteQuery(_)).map(_.sum)
        }
      },
    )
  def cmap[I2](f: I2 => I): QueryI[I2] = QueryI(queryName, fragment, encoder.cmap(f))
}

final case class QueryO[O](queryName: String, fragment: Fragment, decoder: RowDecoder[O]) {
  def apply(): QueryResult[O] = QueryResult.stream(queryName, fragment, None, decoder)
  def map[O2](f: O => O2): QueryO[O2] = QueryO(queryName, fragment, decoder.map(f))
  def emap[O2](f: O => EitherNel[String, O2]): QueryO[O2] = QueryO(queryName, fragment, decoder.emap(f))
}

final case class QueryIO[I, O](queryName: String, fragment: Fragment, encoder: RowEncoder[I], decoder: RowDecoder[O]) {
  def apply(i: I): QueryResult[O] = QueryResult.stream(queryName, fragment, (i, encoder).some, decoder)
  def cmap[I2](f: I2 => I): QueryIO[I2, O] = QueryIO(queryName, fragment, encoder.cmap(f), decoder)
  def map[O2](f: O => O2): QueryIO[I, O2] = QueryIO(queryName, fragment, encoder, decoder.map(f))
  def emap[O2](f: O => EitherNel[String, O2]): QueryIO[I, O2] = QueryIO(queryName, fragment, encoder, decoder.emap(f))
}
