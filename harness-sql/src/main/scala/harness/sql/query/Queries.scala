package harness.sql.query

import cats.data.EitherNel
import cats.syntax.option.*
import harness.sql.*
import harness.sql.errors.*
import harness.sql.typeclass.*
import harness.zio.*
import zio.*

final class Query(queryName: String, sql: String, qim: QueryInputMapper) {
  def apply(): HRIO[JDBCConnection & Logger & Telemetry, Int] =
    ZIO
      .scoped {
        Utils.preparedStatement(sql, None, qim).flatMap { ps =>
          ZIO.hAttempt(ps.executeUpdate()).mapError(ErrorWithSql(sql, _))
        }
      }
      .trace("Executed SQL query", "query-name" -> queryName)
}

final class QueryI[I](queryName: String, val sql: String, encoder: RowEncoder[I], qim: QueryInputMapper) {
  def apply(i: I): HRIO[JDBCConnection & Logger & Telemetry, Int] =
    ZIO
      .scoped {
        Utils.preparedStatement(sql, (i, encoder).some, qim).flatMap { ps =>
          ZIO.hAttempt(ps.executeUpdate()).mapError(ErrorWithSql(sql, _))
        }
      }
      .trace("Executed SQL query", "query-name" -> queryName)
  def batched(is: Chunk[I]): HRIO[JDBCConnection & Logger & Telemetry, Chunk[Int]] =
    ZIO
      .scoped {
        Utils.batchPreparedStatement(sql, encoder, is, qim).flatMap { ps =>
          ZIO.hAttempt { ps.executeBatch() }.mapBoth(ErrorWithSql(sql, _), Chunk.fromArray)
        }
      }
      .trace("Executed SQL query", "query-name" -> queryName)
  def cmap[I2](f: I2 => I): QueryI[I2] = QueryI(queryName, sql, encoder.cmap(f), qim)
}

final class QueryO[O](queryName: String, val sql: String, qim: QueryInputMapper, decoder: RowDecoder[O]) {
  def apply(): QueryResult[O] = QueryResult.stream(queryName, sql, None, qim, decoder)
  def map[O2](f: O => O2): QueryO[O2] = QueryO(queryName, sql, qim, decoder.map(f))
  def emap[O2](f: O => EitherNel[String, O2]): QueryO[O2] = QueryO(queryName, sql, qim, decoder.emap(f))
}

final class QueryIO[I, O](queryName: String, val sql: String, encoder: RowEncoder[I], qim: QueryInputMapper, decoder: RowDecoder[O]) {
  def apply(i: I): QueryResult[O] = QueryResult.stream(queryName, sql, (i, encoder).some, qim, decoder)
  def cmap[I2](f: I2 => I): QueryIO[I2, O] = QueryIO(queryName, sql, encoder.cmap(f), qim, decoder)
  def map[O2](f: O => O2): QueryIO[I, O2] = QueryIO(queryName, sql, encoder, qim, decoder.map(f))
  def emap[O2](f: O => EitherNel[String, O2]): QueryIO[I, O2] = QueryIO(queryName, sql, encoder, qim, decoder.emap(f))
}
