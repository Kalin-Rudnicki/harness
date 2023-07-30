package harness.archive.api.service

import cats.data.NonEmptyList
import harness.archive.api.db.model as M
import harness.archive.api.service.storage.*
import harness.core.*
import harness.sql.{JDBCConnection, JDBCConnectionPool}
import harness.sql.query.Transaction
import harness.zio.*
import zio.*

trait StaleDataCleanser {

  def startFiber: HRIO[Logger & Telemetry, Fiber.Runtime[HError, Unit]]

}
object StaleDataCleanser {

  // TODO (KR) : make backoff configurable
  //           : Another option would be to query db for how long to wait for
  final class Live(
      backoff: NonEmptyList[Duration],
      storageLayer: HRLayer[Scope & Logger, JDBCConnection & Transaction & LogStorage & TraceStorage],
  ) extends StaleDataCleanser {

    private def executeClear: HRIO[LogStorage & TraceStorage & Logger & Telemetry, Boolean] =
      for {
        now <- Clock.currentDateTime
        nowEpoch = now.toInstant.toEpochMilli
        _ <- Logger.log.info(s"Running cleanup @ $now")
        numLogsDeleted <- LogStorage.deleteOutdated(nowEpoch)
        numTracesDeleted <- TraceStorage.deleteOutdated(nowEpoch)
        clearedAnyRecords = numLogsDeleted > 0 || numTracesDeleted > 0
        _ <-
          if (clearedAnyRecords) Logger.log.info(s"Deleted ${numLogsDeleted.pluralizeOn("log")} and ${numTracesDeleted.pluralizeOn("trace")}")
          else Logger.log.debug("No records cleared")
      } yield clearedAnyRecords

    private def executeClearInTransaction: HRIO[LogStorage & TraceStorage & JDBCConnection & Transaction & Logger & Telemetry, Boolean] =
      Transaction.inTransaction { executeClear }

    private def rec(waitNel: NonEmptyList[Duration]): HRIO[Logger & Telemetry, Unit] =
      for {
        _ <- Logger.log.debug(s"Sleeping for ${waitNel.head.prettyPrint}")
        _ <- Clock.sleep(waitNel.head)
        clearedAnyRecords <- ZIO.scoped {
          executeClearInTransaction.provideSomeLayer(storageLayer)
        }
        _ <-
          if (clearedAnyRecords) rec(backoff)
          else rec(NonEmptyList.fromList(waitNel.tail).getOrElse(waitNel))
      } yield ()

    override def startFiber: HRIO[Logger & Telemetry, Fiber.Runtime[HError, Unit]] =
      rec(backoff).fork

  }

  // =====|  |=====

  def startFiber: HRIO[StaleDataCleanser & Logger & Telemetry, Fiber.Runtime[HError, Unit]] =
    ZIO.serviceWithZIO[StaleDataCleanser](_.startFiber)

  // =====|  |=====

  def live(backoff: NonEmptyList[Duration]): URLayer[JDBCConnectionPool, StaleDataCleanser] =
    for {
      poolLayer <- ZLayer.service[JDBCConnectionPool]
    } yield ZEnvironment[StaleDataCleanser](
      Live(
        backoff,
        (ZLayer.succeedEnvironment(poolLayer) >>> JDBCConnection.poolLayer) >+>
          (LogStorage.liveLayer ++ TraceStorage.liveLayer) ++
          ZLayer.succeed(Transaction.Live),
      ),
    )
  def live(backoff0: Duration, backoffN: Duration*): URLayer[JDBCConnectionPool, StaleDataCleanser] = StaleDataCleanser.live(NonEmptyList(backoff0, backoffN.toList))

}
