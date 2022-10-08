package harness.sql.query

import cats.syntax.option.*
import harness.sql.*
import harness.sql.errors.*
import harness.sql.typeclass.*
import java.sql.{Array, PreparedStatement, ResultSet}
import zio.*
import zio.stream.*

final class QueryResult[O] private (sql: String, _stream: => ZStream[JDBCConnection & Scope, Throwable, O]) {

  inline def single: RIO[JDBCConnection, O] =
    chunk.flatMap {
      case Chunk(value) => ZIO.succeed(value)
      case chunk        => ZIO.fail(ErrorWithSql(sql, InvalidResultSetSize("1", chunk.length)))
    }

  inline def option: RIO[JDBCConnection, Option[O]] =
    chunk.flatMap {
      case Chunk(value) => ZIO.some(value)
      case Chunk()      => ZIO.none
      case chunk        => ZIO.fail(ErrorWithSql(sql, InvalidResultSetSize("0..1", chunk.length)))
    }

  inline def list: RIO[JDBCConnection, List[O]] = chunk.map(_.toList)

  inline def chunk: RIO[JDBCConnection, Chunk[O]] = ZIO.scoped { stream.runCollect }

  def stream: ZStream[JDBCConnection & Scope, Throwable, O] = _stream

  // =====|  |=====

  // NOTE : Make sure results are ordered by `K`
  def groupBy[K, V](kf: O => K)(vf: O => V): ZStream[JDBCConnection & Scope, Throwable, (K, NonEmptyChunk[V])] =
    _stream.groupAdjacentBy(kf).map { (k, os) => (k, os.map(vf)) }

  // NOTE : Make sure results are ordered by `K`
  def groupByLeft[K, V](kf: O => K)(vf: O => Option[V]): QueryResult[(K, Chunk[V])] =
    QueryResult(
      sql,
      _stream.groupAdjacentBy(kf).map { case (k, os) => (k, os.map(vf)) }.mapZIO { case (k, chunk) =>
        if (chunk.length == 1 && chunk(0).isEmpty) ZIO.succeed(k, Chunk.empty)
        else if (chunk.forall(_.nonEmpty)) ZIO.succeed((k, chunk.map(_.get)))
        else ZIO.fail(new RuntimeException("GroupBy has unexpected results"))
      },
    )

  // NOTE : Make sure results are ordered by `K`
  def groupByLeftOpt[K, V](kf: O => K)(vf: O => Option[V]): QueryResult[(K, Option[NonEmptyChunk[V]])] =
    QueryResult(
      sql,
      _stream.groupAdjacentBy(kf).map { case (k, os) => (k, os.map(vf)) }.mapZIO { case (k, chunk) =>
        if (chunk.length == 1 && chunk(0).isEmpty) ZIO.succeed(k, None)
        else if (chunk.forall(_.nonEmpty)) ZIO.succeed((k, chunk.map(_.get).some))
        else ZIO.fail(new RuntimeException("GroupBy has unexpected results"))
      },
    )

}
object QueryResult {

  private[query] def stream[I, O](
      sql: String,
      input: Option[(I, RowEncoder[I])],
      qim: QueryInputMapper,
      decoder: RowDecoder[O],
  ): QueryResult[O] =
    QueryResult(
      sql, {
        def resultSet: RIO[JDBCConnection & Scope, ResultSet] =
          for {
            ps <- Utils.preparedStatement(sql, input, qim)
            rs <- Utils.acquireClosable(ps.executeQuery())
          } yield rs

        def result(resultSet: ResultSet): Task[O] =
          for {
            ncs <- ZIO.attempt(resultSet.getMetaData.getColumnCount)
            outputs <-
              if (ncs == decoder.width) ZIO.attempt(IArray.range(0, decoder.width).map(i => resultSet.getObject(i + 1)))
              else ZIO.fail(InvalidResultSetWidth(decoder.width, ncs))
            res <-
              decoder.decodeRow(0, outputs) match {
                case Right(value) => ZIO.succeed(value)
                case Left(errors) => ZIO.fail(RowDecodeFailure(errors))
              }
          } yield res

        ZStream
          .fromZIO(resultSet)
          .flatMap { resultSet =>
            ZStream.repeatZIOOption {
              ZIO.attempt(resultSet.next()).asSomeError.flatMap {
                case true  => result(resultSet).asSomeError
                case false => ZIO.fail(None)
              }
            }
          }
          .mapError(ErrorWithSql(sql, _))
      },
    )

}
