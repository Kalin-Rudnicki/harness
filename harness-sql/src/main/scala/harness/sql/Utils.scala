package harness.sql

import harness.sql.query.QueryInputMapper
import harness.sql.typeclass.*
import java.sql.PreparedStatement
import zio.*

private[sql] object Utils {

  def acquireClosable[C <: AutoCloseable](acq: => C): RIO[Scope, C] =
    ZIO.acquireRelease(ZIO.attempt(acq))(c => ZIO.attempt(c.close()).orDie)

  private def encodeInputs[I](t: I, encoder: RowEncoder[I], queryInputMapper: QueryInputMapper): IArray[Object] = {
    val arr1: Array[Object] = new Array[Object](encoder.width)
    encoder.encodeRow(t, 0, arr1)
    val iArr1: IArray[Object] = IArray.unsafeFromArray(arr1)

    val arr2: Array[Object] = new Array[Object](queryInputMapper.width(iArr1))
    queryInputMapper.prepare(iArr1, arr2, 0)
    val iArr2: IArray[Object] = IArray.unsafeFromArray(arr2)

    iArr2
  }

  def preparedStatement[I](sql: String, input: Option[(I, RowEncoder[I], QueryInputMapper)]): RIO[ConnectionFactory & Scope, PreparedStatement] =
    for {
      connectionFactory <- ZIO.service[ConnectionFactory]
      connection <- connectionFactory.getJDBCConnection
      ps: PreparedStatement <- Utils.acquireClosable { connection.prepareStatement(sql) }
      inputs <-
        ZIO.foreach(input) { (i, enc, qim) =>
          ZIO.attempt { encodeInputs(i, enc, qim) }
        }
      _ <-
        ZIO.foreachDiscard(inputs) { inputs =>
          ZIO.attempt {
            inputs.zipWithIndex.foreach { (input, idx) => ps.setObject(idx + 1, input) }
          }
        }
    } yield ps

}
