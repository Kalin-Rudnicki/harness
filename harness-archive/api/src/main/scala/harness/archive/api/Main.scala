package harness.archive.api

import cats.data.NonEmptyList
import harness.archive.api.db.{model as M, queries as Q}
import harness.archive.api.routes as R
import harness.core.*
import harness.http.server.{given, *}
import harness.sql.*
import harness.sql.autoSchema.*
import harness.sql.query.Transaction
import harness.web.*
import harness.zio.*
import zio.*

object Main extends ExecutableApp {

  type ServerEnv = JDBCConnectionPool & Transaction
  type ReqEnv = JDBCConnection

  // This layer will be evaluated once when the server starts
  val serverLayer: SHRLayer[Scope, ServerEnv] =
    ZLayer.fromZIO { JDBCConnectionPool(ConnectionFactory("jdbc:postgresql:archive", "kalin", "psql-pass"), 4, 16, Duration.fromSeconds(60)) } ++
      ZLayer.succeed(Transaction.Live)

  // This layer will be evaluated for each HTTP request that the server receives
  val reqLayer: SHRLayer[ServerEnv & Scope, ReqEnv] =
    JDBCConnection.poolLayer

  val tables: Tables =
    Tables(
      M.User.tableSchema,
      M.Session.tableSchema,
      M.App.tableSchema,
      M.Log.tableSchema,
      M.Trace.tableSchema,
    )

  def routes(config: ServerConfig): Route[ServerEnv & ReqEnv] =
    Route.stdRoot(config)(
      R.User.routes,
      R.Log.routes,
      R.Telemetry.routes,
    )

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

  private def runClearLoop(originalNel: NonEmptyList[Duration]): HRIO[JDBCConnectionPool & Transaction & Logger & Telemetry, Unit] = {
    def rec(waitNel: NonEmptyList[Duration]): HRIO[JDBCConnectionPool & Transaction & Logger & Telemetry, Unit] =
      for {
        _ <- Logger.log.debug(s"Sleeping for ${waitNel.head.prettyPrint}")
        _ <- Clock.sleep(waitNel.head)
        clearedAnyRecords <- ZIO.scoped { executeClear.provideSomeLayer(JDBCConnection.poolLayer) }
        _ <-
          if (clearedAnyRecords) rec(originalNel)
          else rec(NonEmptyList.fromList(waitNel.tail).getOrElse(waitNel))
      } yield ()

    rec(originalNel).fork.unit
  }
  private def runClearLoop(wait0: Duration, waitN: Duration*): HRIO[JDBCConnectionPool & Transaction & Logger & Telemetry, Unit] =
    runClearLoop(NonEmptyList(wait0, waitN.toList))

  override val executable: Executable =
    Executable
      .withParser(ServerConfig.parser)
      .withLayer[ServerEnv](serverLayer)
      .withEffect { config =>
        PostgresMeta.schemaDiff
          .withPool(tables)
          .mapError(HError.SystemFailure("Failed to execute schema diff", _)) *>
          // TODO (KR) : make backoff configurable
          runClearLoop(5.minutes, 5.minutes, 5.minutes, 15.minutes, 30.minutes) *>
          // runClearLoop(5.seconds, 15.seconds, 1.minute, 2.minutes) *>
          //
          Server.start[ServerEnv, ReqEnv](config, reqLayer) { routes(config) }
      }

}
