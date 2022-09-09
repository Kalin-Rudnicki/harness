package harness.sql.query

import cats.syntax.option.*
import harness.sql.*
import harness.sql.errors.*
import harness.sql.typeclass.*
import java.sql.{Array, PreparedStatement, ResultSet}
import zio.*
import zio.stream.*

final class QueryResult[O] private (sql: String, _stream: => ZStream[ConnectionFactory & Scope, Throwable, O]) {

  inline def single: RIO[ConnectionFactory, O] =
    chunk.flatMap {
      case Chunk(value) => ZIO.succeed(value)
      case chunk        => ZIO.fail(ErrorWithSql(sql, InvalidResultSetSize("1", chunk.length)))
    }

  inline def option: RIO[ConnectionFactory, Option[O]] =
    chunk.flatMap {
      case Chunk(value) => ZIO.some(value)
      case Chunk()      => ZIO.none
      case chunk        => ZIO.fail(ErrorWithSql(sql, InvalidResultSetSize("0..1", chunk.length)))
    }

  inline def list: RIO[ConnectionFactory, List[O]] = chunk.map(_.toList)

  inline def chunk: RIO[ConnectionFactory, Chunk[O]] = ZIO.scoped { stream.runCollect }

  def stream: ZStream[ConnectionFactory & Scope, Throwable, O] = _stream

}
object QueryResult {

  private[query] def stream[I, O](
      sql: String,
      input: Option[(I, RowEncoder[I], QueryInputMapper)],
      decoder: RowDecoder[O],
  ): QueryResult[O] =
    QueryResult(
      sql, {
        def resultSet: RIO[ConnectionFactory & Scope, ResultSet] =
          for {
            ps <- Utils.preparedStatement(sql, input)
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
