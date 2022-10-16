package harness.sql

import harness.sql.query.QueryInputMapper
import harness.sql.typeclass.*
import harness.zio.*
import java.sql.PreparedStatement
import zio.*

private[sql] object Utils {

  def encodeInputs[I](input: Option[(I, RowEncoder[I])], queryInputMapper: QueryInputMapper): IArray[Object] = {
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

  def preparedStatement[I](sql: String, input: Option[(I, RowEncoder[I])], qim: QueryInputMapper): HRIO[JDBCConnection & Scope, PreparedStatement] =
    for {
      connection <- ZIO.service[JDBCConnection]
      ps: PreparedStatement <- ZIO.acquireAutoClosable { ZIO.hAttempt { connection.jdbcConnection.prepareStatement(sql) } }
      inputs <- ZIO.hAttempt { Utils.encodeInputs(input, qim) }
      _ <- ZIO.hAttempt { inputs.zipWithIndex.foreach { (input, idx) => ps.setObject(idx + 1, input) } }
    } yield ps

}
