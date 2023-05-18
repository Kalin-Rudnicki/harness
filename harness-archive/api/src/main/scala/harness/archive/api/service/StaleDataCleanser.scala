package harness.archive.api.service

import cats.data.NonEmptyList
import harness.archive.api.db.{model as M, queries as Q}
import harness.core.*
import harness.sql.{JDBCConnection, JDBCConnectionPool}
import harness.sql.query.Transaction
import harness.zio.*
import zio.*

trait StaleDataCleanser {

  def startFiber: HRIO[JDBCConnectionPool & Transaction & Logger & Telemetry, Unit]

}
object StaleDataCleanser {

  // TODO (KR) : make backoff configurable
  //           : Another option would be to query db for how long to wait for
  final class Live(backoff: NonEmptyList[Duration]) extends StaleDataCleanser {

    private def executeClear: HRIO[JDBCConnection & Transaction & Logger & Telemetry, Boolean] =
      Transaction.inTransaction {
        for {
          now <- Clock.currentDateTime
          nowEpoch = now.toInstant.toEpochMilli
          _ <- Logger.log.info(s"Running cleanup @ $now")
          numLogsDeleted <- Q.Log.deleteOutdated(nowEpoch).execute
          numTracesDeleted <- Q.Log.deleteOutdated(nowEpoch).execute
          clearedAnyRecords = numLogsDeleted > 0 || numTracesDeleted > 0
          _ <-
            if (clearedAnyRecords) Logger.log.info(s"Deleted ${numLogsDeleted.pluralizeOn("log")} and ${numTracesDeleted.pluralizeOn("trace")}")
            else Logger.log.debug("No records cleared")
        } yield clearedAnyRecords
      }

    private def rec(waitNel: NonEmptyList[Duration]): HRIO[JDBCConnectionPool & Transaction & Logger & Telemetry, Unit] =
      for {
        _ <- Logger.log.debug(s"Sleeping for ${waitNel.head.prettyPrint}")
        _ <- Clock.sleep(waitNel.head)
        clearedAnyRecords <- ZIO.scoped {
          executeClear.provideSomeLayer(JDBCConnection.poolLayer)
        }
        _ <-
          if (clearedAnyRecords) rec(backoff)
          else rec(NonEmptyList.fromList(waitNel.tail).getOrElse(waitNel))
      } yield ()

    override def startFiber: HRIO[JDBCConnectionPool & Transaction & Logger & Telemetry, Unit] =
      rec(backoff)

  }

  // =====|  |=====

  def startFiber: HRIO[StaleDataCleanser & JDBCConnectionPool & Transaction & Logger & Telemetry, Unit] =
    ZIO.serviceWithZIO[StaleDataCleanser](_.startFiber)

  // =====|  |=====

  
  def live(backoff: NonEmptyList[Duration]): ULayer[StaleDataCleanser] = ZLayer.succeed { StaleDataCleanser.Live(backoff) }
  def live(backoff0: Duration, backoffN: Duration*): ULayer[StaleDataCleanser] = StaleDataCleanser.live(NonEmptyList(backoff0, backoffN.toList))

}
