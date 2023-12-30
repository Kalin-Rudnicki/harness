package harness.archive.api.service.storage

import harness.archive.api.db.model as M
import harness.sql.*
import harness.sql.query.{given, *}
import harness.zio.*
import zio.*

trait TraceStorage {
  def insertAll(traces: Chunk[M.Trace.Identity]): HRIO[Logger & Telemetry, Unit]
  def forAppId(appId: M.App.Id): HRIO[Logger & Telemetry, Chunk[M.Trace.Identity]]
  def deleteOutdated(now: Long): HRIO[Logger & Telemetry, Int]
  def allForQuery(query: (M.App.Identity, M.Trace.Identity) => Boolean): HRIO[Logger & Telemetry, Chunk[M.Trace.Identity]]
}
object TraceStorage {

  // =====| API |=====

  def insertAll(traces: Chunk[M.Trace.Identity]): HRIO[TraceStorage & Logger & Telemetry, Unit] =
    ZIO.serviceWithZIO[TraceStorage](_.insertAll(traces))

  def forAppId(appId: M.App.Id): HRIO[TraceStorage & Logger & Telemetry, Chunk[M.Trace.Identity]] =
    ZIO.serviceWithZIO[TraceStorage](_.forAppId(appId))

  def deleteOutdated(now: Long): HRIO[TraceStorage & Logger & Telemetry, Int] =
    ZIO.serviceWithZIO[TraceStorage](_.deleteOutdated(now))

  def allForQuery(query: (M.App.Identity, M.Trace.Identity) => Boolean): HRIO[TraceStorage & Logger & Telemetry, Chunk[M.Trace.Identity]] =
    ZIO.serviceWithZIO[TraceStorage](_.allForQuery(query))

  // =====| Live |=====

  val liveLayer: URLayer[JDBCConnection, TraceStorage] =
    ZLayer.fromFunction(new Live(_))

  final class Live(con: JDBCConnection) extends TraceStorage {

    override def insertAll(traces: Chunk[M.Trace.Identity]): HRIO[Logger & Telemetry, Unit] =
      con.use { Q.insert.batched(traces).expectSize(traces.length) }

    override def forAppId(appId: M.App.Id): HRIO[Logger & Telemetry, Chunk[M.Trace.Identity]] =
      con.use { Q.forAppId(appId).chunk }

    override def deleteOutdated(now: Long): HRIO[Logger & Telemetry, Int] =
      con.use { Q.deleteOutdated(now).execute }

    override def allForQuery(query: (M.App.Identity, M.Trace.Identity) => Boolean): HRIO[Logger & Telemetry, Chunk[M.Trace.Identity]] =
      ZIO.scoped { con.use { Q.allWithApp().stream.collect { case (app, trace) if query(app, trace) => trace }.runCollect } }

    // =====| Queries |=====

    private object Q extends TableQueries[M.Trace.Id, M.Trace] {

      val forAppId: QueryIO[M.App.Id, M.Trace.Identity] =
        Prepare.selectIO("Trace - forAppName") { Input[M.App.Id] } { appId =>
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

      val allWithApp: QueryO[(M.App.Identity, M.Trace.Identity)] =
        Prepare.selectO("Trace - allWithApp") {
          Select
            .from[M.Trace]("t")
            .join[M.App]("a")
            .on { case (trace, app) => trace.appId === app.id }
            .returning { case (trace, app) => app ~ trace }
        }

    }

  }

}
