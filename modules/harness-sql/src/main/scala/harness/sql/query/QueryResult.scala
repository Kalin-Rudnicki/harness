package harness.sql.query

import cats.syntax.option.*
import harness.sql.*
import harness.sql.error.QueryError
import harness.sql.typeclass.*
import harness.zio.*
import zio.*
import zio.stream.*

final class QueryResult[O] private (queryName: String, fragment: Fragment, _stream: => ZStream[Database & Scope, QueryError, O]) {

  def single: ZIO[Database, QueryError, O] =
    chunk.flatMap {
      case Chunk(value) => ZIO.succeed(value)
      case chunk        => ZIO.fail(QueryError(queryName, fragment.sql, QueryError.Cause.InvalidResultSetSize("1", chunk.length)))
    }

  def single[E](onMissing: => E)(implicit errorMapper: ErrorMapper[QueryError, E]): ZIO[Database, E, O] =
    option.mapError(errorMapper.mapError).someOrFail(onMissing)

  def option: ZIO[Database, QueryError, Option[O]] =
    chunk.flatMap {
      case Chunk(value) => ZIO.some(value)
      case Chunk()      => ZIO.none
      case chunk        => ZIO.fail(QueryError(queryName, fragment.sql, QueryError.Cause.InvalidResultSetSize("0..1", chunk.length)))
    }

  inline def list: ZIO[Database, QueryError, List[O]] = chunk.map(_.toList)

  inline def chunk: ZIO[Database, QueryError, Chunk[O]] = ZIO.scoped { _stream.runCollect }.telemetrize("Executed SQL query", "query-name" -> queryName)

  def stream: ZStream[Database & Scope, QueryError, O] = _stream

  // =====|  |=====

  // NOTE : Make sure results are ordered by `K`
  def groupBy[K, V](kf: O => K)(vf: O => V): ZStream[Database & Scope, QueryError, (K, NonEmptyChunk[V])] =
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
      input: Option[(I, Input[I, ?])],
      decoder: QueryDecoderMany[O],
  ): QueryResult[O] =
    QueryResult(
      queryName,
      fragment,
      ZStream
        .fromZIO {
          for {
            ps <- PreparedStatement.make(queryName, fragment)
            _ <- ZIO.foreachDiscard(input)(ps.writeSingle(_, _))
            rs <- ps.executeQuery(decoder)
          } yield rs
        }
        .flatMap { resultSet =>
          ZStream.repeatZIOOption { resultSet.decodeRowOpt }
        },
    )

}
