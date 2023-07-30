package harness.archive.api.service.storage

import harness.archive.api.db.model as M
import harness.sql.*
import harness.sql.query.{given, *}
import harness.zio.*
import zio.*

trait LogStorage {
  def insertAll(logs: Chunk[M.Log.Identity]): HRIO[Logger & Telemetry, Unit]
  def byAppId(appId: M.App.Id): HRIO[Logger & Telemetry, Chunk[M.Log.Identity]]
  def deleteOutdated(now: Long): HRIO[Logger & Telemetry, Int]
}
object LogStorage {

  // =====| API |=====

  def insertAll(logs: Chunk[M.Log.Identity]): HRIO[LogStorage & Logger & Telemetry, Unit] =
    ZIO.serviceWithZIO[LogStorage](_.insertAll(logs))

  def byAppId(appId: M.App.Id): HRIO[LogStorage & Logger & Telemetry, Chunk[M.Log.Identity]] =
    ZIO.serviceWithZIO[LogStorage](_.byAppId(appId))

  def deleteOutdated(now: Long): HRIO[LogStorage & Logger & Telemetry, Int] =
    ZIO.serviceWithZIO[LogStorage](_.deleteOutdated(now))

  // =====| Live |=====

  val liveLayer: URLayer[JDBCConnection, LogStorage] =
    ZLayer.fromFunction(new Live(_))

  final class Live(con: JDBCConnection) extends LogStorage {

    override def insertAll(logs: Chunk[M.Log.Identity]): HRIO[Logger & Telemetry, Unit] =
      con.use { Q.insert.batched(logs).single }

    override def byAppId(appId: M.App.Id): HRIO[Logger & Telemetry, Chunk[M.Log.Identity]] =
      con.use { Q.byAppId(appId).chunk }

    override def deleteOutdated(now: Long): HRIO[Logger & Telemetry, Int] =
      con.use { Q.deleteOutdated(now).execute }

    // =====| Queries |=====

    private object Q extends TableQueries[M.Log.Id, M.Log] {

      val byAppId: QueryIO[M.App.Id, M.Log.Identity] =
        Prepare.selectIO("Log - byAppId") { Input[M.App.Id] } { appId =>
          Select
            .from[M.Log]("l")
            .where { l => l.appId === appId }
            .returning { l => l }
        }

      val deleteOutdated: QueryI[Long] =
        Prepare.deleteI("Log - deleteOutdated") { Input[Long] } { now =>
          Delete
            .from[M.Log]("l")
            .where { l => l.keepUntilEpochMS <= now }
        }

    }

  }

}
