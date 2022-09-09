package harness.sql

import harness.sql.query.QueryInputMapper
import harness.sql.typeclass.*
import harness.zio.*
import java.sql.PreparedStatement
import zio.*

private[sql] object Utils {

  private def encodeInputs[I](t: I, encoder: RowEncoder[I], queryInputMapper: QueryInputMapper): IArray[Object] = {
    val arr1: Array[Object] = new Array[Object](encoder.width)
    encoder.encodeRow(t, 0, arr1)
    val iArr1: IArray[Object] = IArray.unsafeFromArray(arr1)

    val arr2: Array[Object] = new Array[Object](queryInputMapper.width(iArr1))
    queryInputMapper.prepare(iArr1, arr2, 0)
    val iArr2: IArray[Object] = IArray.unsafeFromArray(arr2)

    iArr2
  }

  def preparedStatement[I](sql: String, input: Option[(I, RowEncoder[I], QueryInputMapper)]): HRIO[ConnectionFactory & Scope, PreparedStatement] =
    for {
      connectionFactory <- ZIO.service[ConnectionFactory]
      connection <- connectionFactory.getJDBCConnection
      ps: PreparedStatement <-
        ZIO
          .acquireRelease(ZIO.hAttempt("Unable to create PreparedStatement")(connection.prepareStatement(sql)))
          .apply(ps => ZIO.hAttempt("Unable to close PreparedStatement")(ps.close()).orDieH)
      inputs <-
        ZIO.foreach(input) { (i, enc, qim) =>
          ZIO.hAttempt(s"Unable to create PreparedStatement inputs") { encodeInputs(i, enc, qim) }
        }
      _ <-
        ZIO.foreachDiscard(inputs) { inputs =>
          ZIO.hAttempt(s"Unable to set PreparedStatement inputs: ${inputs.mkString("[", ", ", "]")}") {
            inputs.zipWithIndex.foreach { (input, idx) => ps.setObject(idx + 1, input) }
          }
        }
    } yield ps

}
