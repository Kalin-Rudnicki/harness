package harness.sql.query

import cats.data.EitherNel
import cats.syntax.option.*
import harness.sql.*
import harness.sql.errors.*
import harness.sql.typeclass.*
import zio.*

final class Query(sql: String, qim: QueryInputMapper) {
  def apply(): RIO[JDBCConnection, Int] =
    ZIO.scoped {
      Utils.preparedStatement(sql, None, qim).flatMap { ps =>
        ZIO.attempt(ps.executeUpdate()).mapError(ErrorWithSql(sql, _))
      }
    }
}

final class QueryI[I](sql: String, encoder: RowEncoder[I], qim: QueryInputMapper) {
  def apply(i: I): RIO[JDBCConnection, Int] =
    ZIO.scoped {
      Utils.preparedStatement(sql, (i, encoder).some, qim).flatMap { ps =>
        ZIO.attempt(ps.executeUpdate()).mapError(ErrorWithSql(sql, _))
      }
    }
  def cmap[I2](f: I2 => I): QueryI[I2] = QueryI(sql, encoder.cmap(f), qim)
}

final class QueryO[O](sql: String, qim: QueryInputMapper, decoder: RowDecoder[O]) {
  def apply(): QueryResult[O] = QueryResult.stream(sql, None, qim, decoder)
  def map[O2](f: O => O2): QueryO[O2] = QueryO(sql, qim, decoder.map(f))
  def emap[O2](f: O => EitherNel[String, O2]): QueryO[O2] = QueryO(sql, qim, decoder.emap(f))
}

final class QueryIO[I, O](sql: String, encoder: RowEncoder[I], qim: QueryInputMapper, decoder: RowDecoder[O]) {
  def apply(i: I): QueryResult[O] = QueryResult.stream(sql, (i, encoder).some, qim, decoder)
  def cmap[I2](f: I2 => I): QueryIO[I2, O] = QueryIO(sql, encoder.cmap(f), qim, decoder)
  def map[O2](f: O => O2): QueryIO[I, O2] = QueryIO(sql, encoder, qim, decoder.map(f))
  def emap[O2](f: O => EitherNel[String, O2]): QueryIO[I, O2] = QueryIO(sql, encoder, qim, decoder.emap(f))
}
