package harness.sql.query

import cats.data.EitherNel
import cats.syntax.option.*
import harness.sql.*
import harness.sql.errors.*
import harness.sql.typeclass.*
import zio.*

final class Query private[query] (sql: String) {
  def apply(): RIO[ConnectionFactory, Int] =
    ZIO.scoped {
      Utils.preparedStatement(sql, None).flatMap { ps =>
        ZIO.attempt(ps.executeUpdate()).mapError(ErrorWithSql(sql, _))
      }
    }
}

final class QueryI[I] private[query] (sql: String, encoder: RowEncoder[I], qim: QueryInputMapper) {
  def apply(i: I): RIO[ConnectionFactory, Int] =
    ZIO.scoped {
      Utils.preparedStatement(sql, (i, encoder, qim).some).flatMap { ps =>
        ZIO.attempt(ps.executeUpdate()).mapError(ErrorWithSql(sql, _))
      }
    }
  def cmap[I2](f: I2 => I): QueryI[I2] = QueryI(sql, encoder.cmap(f), qim)
}

final class QueryO[O] private[query] (sql: String, decoder: RowDecoder[O]) {
  def apply(): QueryResult[O] =
    QueryResult.stream(sql, None, decoder)
  def map[O2](f: O => O2): QueryO[O2] = QueryO(sql, decoder.map(f))
  def emap[O2](f: O => EitherNel[String, O2]): QueryO[O2] = QueryO(sql, decoder.emap(f))
}

final class QueryIO[I, O] private[query] (sql: String, encoder: RowEncoder[I], qim: QueryInputMapper, decoder: RowDecoder[O]) {
  def apply(i: I): QueryResult[O] =
    QueryResult.stream(sql, (i, encoder, qim).some, decoder)
  def cmap[I2](f: I2 => I): QueryIO[I2, O] = QueryIO(sql, encoder.cmap(f), qim, decoder)
  def map[O2](f: O => O2): QueryIO[I, O2] = QueryIO(sql, encoder, qim, decoder.map(f))
  def emap[O2](f: O => EitherNel[String, O2]): QueryIO[I, O2] = QueryIO(sql, encoder, qim, decoder.emap(f))
}
