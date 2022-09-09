package harness.sql.query

import cats.syntax.option.*
import harness.core.*
import harness.sql.*
import harness.sql.typeclass.*
import harness.zio.*
import java.sql.{Array, PreparedStatement, ResultSet}
import zio.*
import zio.stream.*

final class QueryResult[O] private (sql: String, _stream: => ZStream[ConnectionFactory & Scope, HError, O]) {

  inline def single: HRIO[ConnectionFactory, O] =
    chunk.flatMap {
      case Chunk(value) => ZIO.succeed(value)
      case chunk        => ZIO.fail(HError.InternalDefect(s"Expected [1] result, but got ${chunk.length}\nsql: $sql"))
    }

  inline def option: HRIO[ConnectionFactory, Option[O]] =
    chunk.flatMap {
      case Chunk(value) => ZIO.some(value)
      case Chunk()      => ZIO.none
      case chunk        => ZIO.fail(HError.InternalDefect(s"Expected [0..1] result, but got ${chunk.length}\nsql: $sql"))
    }

  inline def list: HRIO[ConnectionFactory, List[O]] = chunk.map(_.toList)

  inline def chunk: HRIO[ConnectionFactory, Chunk[O]] = ZIO.scoped { stream.runCollect }

  def stream: ZStream[ConnectionFactory & Scope, HError, O] = _stream

}
object QueryResult {

  private[query] def stream[I, O](
      sql: String,
      input: Option[(I, RowEncoder[I], QueryInputMapper)],
      decoder: RowDecoder[O],
  ): QueryResult[O] =
    QueryResult(
      sql, {
        def resultSet: HRIO[ConnectionFactory & Scope, ResultSet] =
          for {
            ps: PreparedStatement <- Utils.preparedStatement(sql, input)
            rs: ResultSet <-
              ZIO
                .acquireRelease(ZIO.hAttempt("Unable to create ResultSet")(ps.executeQuery()))
                .apply(rs => ZIO.hAttempt("Unable to close ResultSet")(rs.close()).orDieH)
          } yield rs

        def result(resultSet: ResultSet): IO[HError, O] =
          for {
            ncs <- ZIO.hAttempt("Unable to get ResultSet column count")(resultSet.getMetaData.getColumnCount)
            outputs <-
              if (ncs == decoder.width) ZIO.hAttempt("Unable to get ResultSet outputs")(IArray.range(0, decoder.width).map(i => resultSet.getObject(i + 1)))
              else ZIO.fail(HError.InternalDefect(s"ResultSet width ($ncs) is different from expected (${decoder.width})"))
            res <-
              decoder.decodeRow(0, outputs) match {
                case Right(value) => ZIO.succeed(value)
                case Left(errors) => ZIO.fail(HError.InternalDefect(s"Unable to decode row: ${errors.toList.mkString("[", ", ", "]")}"))
              }
          } yield res

        ZStream.fromZIO(resultSet).flatMap { resultSet =>
          ZStream.repeatZIOOption {
            ZIO.hAttempt("Unable to get ResultSet.next")(resultSet.next()).asSomeError.flatMap {
              case true  => result(resultSet).asSomeError
              case false => ZIO.fail(None)
            }
          }
        }
      },
    )

}
