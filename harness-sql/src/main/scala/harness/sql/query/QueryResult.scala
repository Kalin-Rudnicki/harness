package harness.sql.query

import cats.syntax.option.*
import harness.core.*
import harness.sql.*
import harness.sql.errors.*
import harness.sql.typeclass.*
import harness.zio.*
import java.sql.{Array, PreparedStatement, ResultSet}
import zio.*
import zio.stream.*

final class QueryResult[O] private (sql: String, _stream: => HRStream[JDBCConnection & Logger & Scope, O]) {

  inline def single: HRIO[JDBCConnection & Logger, O] =
    chunk.flatMap {
      case Chunk(value) => ZIO.succeed(value)
      case chunk        => ZIO.fail(ErrorWithSql(sql, InvalidResultSetSize("1", chunk.length)))
    }

  inline def option: HRIO[JDBCConnection & Logger, Option[O]] =
    chunk.flatMap {
      case Chunk(value) => ZIO.some(value)
      case Chunk()      => ZIO.none
      case chunk        => ZIO.fail(ErrorWithSql(sql, InvalidResultSetSize("0..1", chunk.length)))
    }

  inline def list: HRIO[JDBCConnection & Logger, List[O]] = chunk.map(_.toList)

  inline def chunk: HRIO[JDBCConnection & Logger, Chunk[O]] = ZIO.scoped { stream.runCollect }

  def stream: HRStream[JDBCConnection & Logger & Scope, O] = _stream

  // =====|  |=====

  // NOTE : Make sure results are ordered by `K`
  def groupBy[K, V](kf: O => K)(vf: O => V): HRStream[JDBCConnection & Logger & Scope, (K, NonEmptyChunk[V])] =
    _stream.groupAdjacentBy(kf).map { (k, os) => (k, os.map(vf)) }

  // NOTE : Make sure results are ordered by `K`
  def groupByLeft[K, V](kf: O => K)(vf: O => Option[V]): QueryResult[(K, Chunk[V])] =
    QueryResult(
      sql,
      _stream.groupAdjacentBy(kf).map { case (k, os) => (k, os.map(vf)) }.mapZIO { case (k, chunk) =>
        if (chunk.length == 1 && chunk(0).isEmpty) ZIO.succeed(k, Chunk.empty)
        else if (chunk.forall(_.nonEmpty)) ZIO.succeed((k, chunk.map(_.get)))
        else ZIO.fail(HError.InternalDefect("GroupBy has unexpected results"))
      },
    )

  // NOTE : Make sure results are ordered by `K`
  def groupByLeftOpt[K, V](kf: O => K)(vf: O => Option[V]): QueryResult[(K, Option[NonEmptyChunk[V]])] =
    QueryResult(
      sql,
      _stream.groupAdjacentBy(kf).map { case (k, os) => (k, os.map(vf)) }.mapZIO { case (k, chunk) =>
        if (chunk.length == 1 && chunk(0).isEmpty) ZIO.succeed(k, None)
        else if (chunk.forall(_.nonEmpty)) ZIO.succeed((k, chunk.map(_.get).some))
        else ZIO.fail(HError.InternalDefect("GroupBy has unexpected results"))
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
        def resultSet: HRIO[JDBCConnection & Logger & Scope, ResultSet] =
          for {
            ps <- Utils.preparedStatement(sql, input, qim)
            rs <- ZIO.acquireAutoClosable(ZIO.hAttempt(ps.executeQuery()))
          } yield rs

        def result(resultSet: ResultSet): HTask[O] =
          for {
            ncs <- ZIO.hAttempt(resultSet.getMetaData.getColumnCount)
            outputs <-
              if (ncs == decoder.width) ZIO.hAttempt(IArray.range(0, decoder.width).map(i => resultSet.getObject(i + 1)))
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
              ZIO.hAttempt(resultSet.next()).asSomeError.flatMap {
                case true  => result(resultSet).asSomeError
                case false => ZIO.fail(None)
              }
            }
          }
          .mapError(ErrorWithSql(sql, _))
      },
    )

}
