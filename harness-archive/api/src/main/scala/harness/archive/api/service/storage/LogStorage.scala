package harness.archive.api.service.storage

import harness.archive.api.db.model as M
import harness.sql.*
import harness.sql.query.{given, *}
import harness.zio.*
import zio.*

trait LogStorage {
  def insertAll(logs: Chunk[M.Log.Identity]): HRIO[Logger & Telemetry, Unit]
  def forAppId(appId: M.App.Id): HRIO[Logger & Telemetry, Chunk[M.Log.Identity]]
  def deleteOutdated(now: Long): HRIO[Logger & Telemetry, Int]
  def allForQuery(userId: M.User.Id, query: (M.App.Identity, M.Log.Identity) => Boolean): HRIO[Logger & Telemetry, Chunk[M.Log.Identity]]
}
object LogStorage {

  // =====| API |=====

  def insertAll(logs: Chunk[M.Log.Identity]): HRIO[LogStorage & Logger & Telemetry, Unit] =
    ZIO.serviceWithZIO[LogStorage](_.insertAll(logs))

  def forAppId(appId: M.App.Id): HRIO[LogStorage & Logger & Telemetry, Chunk[M.Log.Identity]] =
    ZIO.serviceWithZIO[LogStorage](_.forAppId(appId))

  def deleteOutdated(now: Long): HRIO[LogStorage & Logger & Telemetry, Int] =
    ZIO.serviceWithZIO[LogStorage](_.deleteOutdated(now))

  def allForQuery(userId: M.User.Id, query: (M.App.Identity, M.Log.Identity) => Boolean): HRIO[LogStorage & Logger & Telemetry, Chunk[M.Log.Identity]] =
    ZIO.serviceWithZIO[LogStorage](_.allForQuery(userId, query))

  // =====| Live |=====

  val liveLayer: URLayer[JDBCConnection, LogStorage] =
    ZLayer.fromFunction(new Live(_))

  final class Live(con: JDBCConnection) extends LogStorage {

    override def insertAll(logs: Chunk[M.Log.Identity]): HRIO[Logger & Telemetry, Unit] =
      con.use { Q.insert.batched(logs).expectSize(logs.length) }

    override def forAppId(appId: M.App.Id): HRIO[Logger & Telemetry, Chunk[M.Log.Identity]] =
      con.use { Q.forAppId(appId).chunk }

    override def deleteOutdated(now: Long): HRIO[Logger & Telemetry, Int] =
      con.use { Q.deleteOutdated(now).execute }

    override def allForQuery(userId: M.User.Id, query: (M.App.Identity, M.Log.Identity) => Boolean): HRIO[Logger & Telemetry, Chunk[M.Log.Identity]] =
      ZIO.scoped { con.use { Q.allWithApp(userId).stream.collect { case (app, log) if query(app, log) => log }.runCollect } }

    // =====| Queries |=====

    private object Q extends TableQueries[M.Log.Id, M.Log] {

      val forAppId: QueryIO[M.App.Id, M.Log.Identity] =
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

      val allWithApp: QueryIO[M.User.Id, (M.App.Identity, M.Log.Identity)] =
        Prepare.selectIO("Log - allWithApp") { Input[M.User.Id] } { userId =>
          Select
            .from[M.App]("a")
            .join[M.Log]("l")
            .on { case (app, log) => log.appId === app.id }
            .where { case (app, _) => app.userId === userId }
            .returning { case (app, log) => app ~ log }
        }

    }

  }

}
