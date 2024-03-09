package harness.sql.query

import cats.syntax.option.*
import harness.sql.*
import harness.sql.error.QueryError
import harness.sql.typeclass.*
import harness.zio.*
import java.sql.ResultSet
import zio.*
import zio.stream.*

final class QueryResult[O] private (queryName: String, fragment: Fragment, _stream: => ZStream[JDBCConnection & Logger & Scope, QueryError, O]) {

  def single: ZIO[JDBCConnection & Logger & Telemetry, QueryError, O] =
    chunk.flatMap {
      case Chunk(value) => ZIO.succeed(value)
      case chunk        => ZIO.fail(QueryError(queryName, fragment.sql, QueryError.Cause.InvalidResultSetSize("1", chunk.length)))
    }

  def single[E](onMissing: => E)(implicit errorMapper: ErrorMapper[QueryError, E]): ZIO[JDBCConnection & Logger & Telemetry, E, O] =
    option.mapError(errorMapper.mapError).someOrFail(onMissing)

  def option: ZIO[JDBCConnection & Logger & Telemetry, QueryError, Option[O]] =
    chunk.flatMap {
      case Chunk(value) => ZIO.some(value)
      case Chunk()      => ZIO.none
      case chunk        => ZIO.fail(QueryError(queryName, fragment.sql, QueryError.Cause.InvalidResultSetSize("0..1", chunk.length)))
    }

  inline def list: ZIO[JDBCConnection & Logger & Telemetry, QueryError, List[O]] = chunk.map(_.toList)

  inline def chunk: ZIO[JDBCConnection & Logger & Telemetry, QueryError, Chunk[O]] = ZIO.scoped { stream.runCollect }.telemetrize("Executed SQL query", "query-name" -> queryName)

  def stream: ZStream[JDBCConnection & Logger & Scope, QueryError, O] = _stream

  // =====|  |=====

  // NOTE : Make sure results are ordered by `K`
  def groupBy[K, V](kf: O => K)(vf: O => V): ZStream[JDBCConnection & Logger & Scope, QueryError, (K, NonEmptyChunk[V])] =
    _stream.groupAdjacentBy(kf).map { (k, os) => (k, os.map(vf)) }

  // NOTE : Make sure results are ordered by `K`
  def groupByLeft[K, V](kf: O => K)(vf: O => Option[V]): QueryResult[(K, Chunk[V])] =
    QueryResult(
      queryName,
      fragment,
      _stream.groupAdjacentBy(kf).map { case (k, os) => (k, os.map(vf)) }.mapZIO { case (k, chunk) =>
        if (chunk.length == 1 && chunk(0).isEmpty) ZIO.succeed(k, Chunk.empty)
        else if (chunk.forall(_.nonEmpty)) ZIO.succeed((k, chunk.map(_.get)))
        else ZIO.dieMessage("GroupBy has unexpected results")
      },
    )

  // NOTE : Make sure results are ordered by `K`
  def groupByLeftOpt[K, V](kf: O => K)(vf: O => Option[V]): QueryResult[(K, Option[NonEmptyChunk[V]])] =
    QueryResult(
      queryName,
      fragment,
      _stream.groupAdjacentBy(kf).map { case (k, os) => (k, os.map(vf)) }.mapZIO { case (k, chunk) =>
        if (chunk.length == 1 && chunk(0).isEmpty) ZIO.succeed(k, None)
        else if (chunk.forall(_.nonEmpty)) ZIO.succeed((k, chunk.map(_.get).some))
        else ZIO.dieMessage("GroupBy has unexpected results")
      },
    )

}
object QueryResult {

  private[query] def stream[I, O](
      queryName: String,
      fragment: Fragment,
      input: Option[(I, RowEncoder[I])],
      decoder: RowDecoder[O],
  ): QueryResult[O] =
    QueryResult(
      queryName,
      fragment, {
        def resultSet: ZIO[JDBCConnection & Logger & Scope, QueryError, ResultSet] =
          for {
            ps <- Utils.preparedStatement(queryName, fragment, input)
            rs <- ZIO.acquireAutoClosable(QueryError.attempt(queryName, fragment.sql) { ps.executeQuery() }(QueryError.Cause.UnableToExecuteQuery(_)))
          } yield rs

        inline def getObj(resultSet: ResultSet, k: Option[Class[?]], i: Int): Object =
          k match {
            case Some(k) => resultSet.getObject(i + 1, k)
            case None    => resultSet.getObject(i + 1)
          }

        def result(resultSet: ResultSet): IO[QueryError, O] =
          for {
            ncs <- QueryError.attempt(queryName, fragment.sql)(resultSet.getMetaData.getColumnCount)(QueryError.Cause.Generic("Unable to get result width", _))
            outputs <-
              if (ncs == decoder.width)
                QueryError.attempt(queryName, fragment.sql) { decoder.classes.zipWithIndex.map(getObj(resultSet, _, _)) }(QueryError.Cause.Generic("Unable to get result objects", _))
              else ZIO.fail(QueryError(queryName, fragment.sql, QueryError.Cause.InvalidResultSetWidth(decoder.width, ncs)))
            res <-
              decoder.decodeRow(0, outputs) match {
                case Right(value) => ZIO.succeed(value)
                case Left(errors) => ZIO.fail(QueryError(queryName, fragment.sql, QueryError.Cause.RowDecodeFailure(errors, outputs)))
              }
          } yield res

        ZStream
          .fromZIO(resultSet)
          .flatMap { resultSet =>
            ZStream.repeatZIOOption {
              QueryError.attempt(queryName, fragment.sql)(resultSet.next())(QueryError.Cause.Generic("Unable to get query next", _)).asSomeError.flatMap {
                case true  => result(resultSet).asSomeError
                case false => ZIO.fail(None)
              }
            }
          }
      },
    )

}
