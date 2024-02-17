package harness.sql

import cats.syntax.option.*
import harness.sql.error.QueryError
import harness.sql.query.{Fragment, QueryInputMapper}
import harness.sql.typeclass.*
import harness.zio.*
import java.sql.PreparedStatement
import zio.*

private[sql] object Utils {

  private inline def encodeInputs[I](input: Option[(I, RowEncoder[I])], queryInputMapper: QueryInputMapper): IArray[Object] = {
    val arr1: Array[Object] =
      input match {
        case Some((t, encoder)) =>
          val tmp: Array[Object] = new Array[Object](encoder.width)
          encoder.encodeRow(t, 0, tmp)
          tmp
        case None =>
          new Array[Object](0)
      }
    val iArr1: IArray[Object] = IArray.unsafeFromArray(arr1)

    val arr2: Array[Object] = new Array[Object](queryInputMapper.width(iArr1))
    queryInputMapper.prepare(iArr1, arr2, 0)
    val iArr2: IArray[Object] = IArray.unsafeFromArray(arr2)

    iArr2
  }

  private inline def writeInputs[I](
      ps: PreparedStatement,
      queryName: String,
      fragment: Fragment,
      input: Option[(I, RowEncoder[I])],
  ): ZIO[Logger, QueryError, Unit] =
    for {
      inputs <- QueryError.attempt(queryName, fragment.sql) { Utils.encodeInputs(input, fragment.qim) }(QueryError.Cause.Generic("Unable to encode inputs", _))
      _ <- QueryError.attempt(queryName, fragment.sql) { inputs.zipWithIndex.foreach { (input, idx) => ps.setObject(idx + 1, input) } }(QueryError.Cause.Generic("Unable to set inputs", _))
      // TODO (KR) : remove this? not safe?
      _ <- Logger.log.trace(s"SQL:\n  ${fragment.sql}\nInputs:${inputs.zipWithIndex.map { (input, idx) => s"\n  $$${idx + 1} : $input" }.mkString}")
    } yield ()

  def preparedStatement[I](queryName: String, fragment: Fragment, input: Option[(I, RowEncoder[I])]): ZIO[JDBCConnection & Logger & Scope, QueryError, PreparedStatement] =
    for {
      connection <- ZIO.service[JDBCConnection]
      ps: PreparedStatement <- ZIO.acquireAutoClosable {
        QueryError.attempt(queryName, fragment.sql) { connection.jdbcConnection.prepareStatement(fragment.sql) }(QueryError.Cause.Generic("Unable to create prepared statement", _))
      }
      _ <- writeInputs(ps, queryName, fragment, input)
    } yield ps

  def batchPreparedStatement[I](queryName: String, fragment: Fragment, rowEncoder: RowEncoder[I], inputs: Chunk[I]): ZIO[JDBCConnection & Logger & Scope, QueryError, PreparedStatement] =
    for {
      connection <- ZIO.service[JDBCConnection]
      ps: PreparedStatement <- ZIO.acquireAutoClosable {
        QueryError.attempt(queryName, fragment.sql) { connection.jdbcConnection.prepareStatement(fragment.sql) }(QueryError.Cause.Generic("Unable to create prepared statement", _))
      }
      _ <- ZIO.foreachDiscard(inputs) { input =>
        // TODO (KR) : This call to 'encodeInputs' could possibly be made more efficient
        writeInputs(ps, queryName, fragment, (input, rowEncoder).some) *>
          QueryError.attempt(queryName, fragment.sql) {
            ps.addBatch()
            ps.clearParameters()
          }(QueryError.Cause.Generic("Unable to add batch", _))
      }
      // TODO (KR) : logging?
    } yield ps

}
