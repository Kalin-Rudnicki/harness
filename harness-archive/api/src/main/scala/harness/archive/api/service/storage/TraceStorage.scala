package harness.archive.api.service.storage

import harness.archive.api.db.model as M
import harness.sql.*
import harness.sql.query.{given, *}
import harness.zio.*
import zio.*

trait TraceStorage {
  def insertAll(traces: Chunk[M.Trace.Identity]): HRIO[JDBCConnection & Logger & Telemetry, Unit]
  def byAppId(appId: M.App.Id): HRIO[JDBCConnection & Logger & Telemetry, Chunk[M.Trace.Identity]]
  def deleteOutdated(now: Long): HRIO[JDBCConnection & Logger & Telemetry, Int]
}
object TraceStorage {

  // =====| API |=====

  def insertAll(traces: Chunk[M.Trace.Identity]): HRIO[TraceStorage & JDBCConnection & Logger & Telemetry, Unit] =
    ZIO.serviceWithZIO[TraceStorage](_.insertAll(traces))

  def byAppId(appId: M.App.Id): HRIO[TraceStorage & JDBCConnection & Logger & Telemetry, Chunk[M.Trace.Identity]] =
    ZIO.serviceWithZIO[TraceStorage](_.byAppId(appId))

  def deleteOutdated(now: Long): HRIO[TraceStorage & JDBCConnection & Logger & Telemetry, Int] =
    ZIO.serviceWithZIO[TraceStorage](_.deleteOutdated(now))

  // =====| Live |=====

  val liveLayer: ULayer[TraceStorage] = ZLayer.succeed(new Live)

  final class Live extends TraceStorage {

    override def insertAll(traces: Chunk[M.Trace.Identity]): HRIO[JDBCConnection & Logger & Telemetry, Unit] =
      Q.insert.batched(traces).single

    override def byAppId(appId: M.App.Id): HRIO[JDBCConnection & Logger & Telemetry, Chunk[M.Trace.Identity]] =
      Q.byAppId(appId).chunk

    override def deleteOutdated(now: Long): HRIO[JDBCConnection & Logger & Telemetry, Int] =
      Q.deleteOutdated(now).execute

    // =====| Queries |=====

    private object Q extends TableQueries[M.Trace.Id, M.Trace] {

      val byAppId: QueryIO[M.App.Id, M.Trace.Identity] =
        Prepare.selectIO("Trace - byAppName") { Input[M.App.Id] } { appId =>
          Select
            .from[M.Trace]("t")
            .where { t => t.appId === appId }
            .returning { t => t }
        }

      val deleteOutdated: QueryI[Long] =
        Prepare.deleteI("Trace - deleteOutdated") { Input[Long] } { now =>
          Delete
            .from[M.Trace]("t")
            .where { t => t.keepUntilEpochMS <= now }
        }

    }

  }

}
