package harness.sql.query

import cats.data.EitherNel
import cats.syntax.option.*
import harness.sql.*
import harness.sql.errors.*
import harness.sql.typeclass.*
import harness.zio.*
import zio.*

final class Query(queryName: String, val fragment: Fragment) {
  def apply(): IntQueryResult =
    IntQueryResult(
      queryName,
      fragment,
      ZIO.scoped {
        Utils
          .preparedStatement(fragment, None)
          .flatMap { ps => ZIO.hAttempt(ps.executeUpdate()) }
      },
    )
}

final class QueryI[I](queryName: String, val fragment: Fragment, encoder: RowEncoder[I]) {
  def apply(i: I): IntQueryResult =
    IntQueryResult(
      queryName,
      fragment,
      ZIO.scoped {
        Utils.preparedStatement(fragment, (i, encoder).some).flatMap { ps =>
          ZIO.hAttempt(ps.executeUpdate()).mapError(ErrorWithSql(fragment.sql, _))
        }
      },
    )
  def batched(is: Chunk[I]): IntQueryResult =
    IntQueryResult(
      queryName,
      fragment,
      ZIO.scoped {
        Utils.batchPreparedStatement(fragment, encoder, is).flatMap { ps =>
          ZIO.hAttempt { ps.executeBatch() }.mapBoth(ErrorWithSql(fragment.sql, _), _.sum)
        }
      },
    )
  def cmap[I2](f: I2 => I): QueryI[I2] = QueryI(queryName, fragment, encoder.cmap(f))
}

final class QueryO[O](queryName: String, val fragment: Fragment, decoder: RowDecoder[O]) {
  def apply(): QueryResult[O] = QueryResult.stream(queryName, fragment, None, decoder)
  def map[O2](f: O => O2): QueryO[O2] = QueryO(queryName, fragment, decoder.map(f))
  def emap[O2](f: O => EitherNel[String, O2]): QueryO[O2] = QueryO(queryName, fragment, decoder.emap(f))
}

final class QueryIO[I, O](queryName: String, val fragment: Fragment, encoder: RowEncoder[I], decoder: RowDecoder[O]) {
  def apply(i: I): QueryResult[O] = QueryResult.stream(queryName, fragment, (i, encoder).some, decoder)
  def cmap[I2](f: I2 => I): QueryIO[I2, O] = QueryIO(queryName, fragment, encoder.cmap(f), decoder)
  def map[O2](f: O => O2): QueryIO[I, O2] = QueryIO(queryName, fragment, encoder, decoder.map(f))
  def emap[O2](f: O => EitherNel[String, O2]): QueryIO[I, O2] = QueryIO(queryName, fragment, encoder, decoder.emap(f))
}
