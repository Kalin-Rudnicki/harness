package harness.archive.api.service.storage

import harness.archive.api.db.model as M
import harness.sql.*
import harness.sql.query.{given, *}
import harness.zio.*
import zio.*

trait AppStorage {
  def insert(app: M.App.Identity): HRIO[JDBCConnection & Logger & Telemetry, Unit]
  def insertAll(apps: Chunk[M.App.Identity]): HRIO[JDBCConnection & Logger & Telemetry, Unit]
  def byName(name: String): HRIO[JDBCConnection & Logger & Telemetry, Option[M.App.Identity]]
  def selectAll: HRIO[JDBCConnection & Logger & Telemetry, Chunk[M.App.Identity]]
}
object AppStorage {

  // =====| API |=====

  def insert(app: M.App.Identity): HRIO[AppStorage & JDBCConnection & Logger & Telemetry, Unit] =
    ZIO.serviceWithZIO[AppStorage](_.insert(app))

  def insertAll(apps: Chunk[M.App.Identity]): HRIO[AppStorage & JDBCConnection & Logger & Telemetry, Unit] =
    ZIO.serviceWithZIO[AppStorage](_.insertAll(apps))

  def byName(name: String): HRIO[AppStorage & JDBCConnection & Logger & Telemetry, Option[M.App.Identity]] =
    ZIO.serviceWithZIO[AppStorage](_.byName(name))

  def selectAll: HRIO[AppStorage & JDBCConnection & Logger & Telemetry, Chunk[M.App.Identity]] =
    ZIO.serviceWithZIO[AppStorage](_.selectAll)

  // =====| Live |=====

  val liveLayer: ULayer[AppStorage] = ZLayer.succeed(new Live)

  final class Live extends AppStorage {

    override def insert(app: M.App.Identity): HRIO[JDBCConnection & Logger & Telemetry, Unit] =
      Q.insert(app).single

    override def insertAll(apps: Chunk[M.App.Identity]): HRIO[JDBCConnection & Logger & Telemetry, Unit] =
      Q.insert.batched(apps).single

    override def byName(name: String): HRIO[JDBCConnection & Logger & Telemetry, Option[M.App.Identity]] =
      Q.byName(name).option

    override def selectAll: HRIO[JDBCConnection & Logger & Telemetry, Chunk[M.App.Identity]] =
      Q.selectAll().chunk

    // =====| Queries |=====

    private object Q extends TableQueries[M.App.Id, M.App] {

      val byName: QueryIO[String, M.App.Identity] =
        Prepare.selectIO("App - byName") {
          Input[String]
        } { appName =>
          Select
            .from[M.App]("a")
            .where { a => a.name === appName }
            .returning { a => a }
        }

    }

  }

}
