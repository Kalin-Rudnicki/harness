package harness.archive.api.service.storage

import harness.archive.api.db.model as M
import harness.sql.*
import harness.sql.query.{given, *}
import harness.zio.*
import zio.*

trait AppStorage {
  def insert(app: M.App.Identity): HRIO[Logger & Telemetry, Unit]
  def byId(id: M.App.Id): HRIO[Logger & Telemetry, M.App.Identity]
  def byName(userId: M.User.Id, name: String): HRIO[Logger & Telemetry, Option[M.App.Identity]]
  def selectAll(userId: M.User.Id): HRIO[Logger & Telemetry, Chunk[M.App.Identity]]
}
object AppStorage {

  // =====| API |=====

  def insert(app: M.App.Identity): HRIO[AppStorage & Logger & Telemetry, Unit] =
    ZIO.serviceWithZIO[AppStorage](_.insert(app))

  def byId(id: M.App.Id): HRIO[AppStorage & Logger & Telemetry, M.App.Identity] =
    ZIO.serviceWithZIO[AppStorage](_.byId(id))

  def byName(userId: M.User.Id, name: String): HRIO[AppStorage & Logger & Telemetry, Option[M.App.Identity]] =
    ZIO.serviceWithZIO[AppStorage](_.byName(userId, name))

  def selectAll(userId: M.User.Id): HRIO[AppStorage & Logger & Telemetry, Chunk[M.App.Identity]] =
    ZIO.serviceWithZIO[AppStorage](_.selectAll(userId))

  // =====| Live |=====

  val liveLayer: URLayer[JDBCConnection, AppStorage] =
    ZLayer.fromFunction(new Live(_))

  final class Live(con: JDBCConnection) extends AppStorage {

    override def insert(app: M.App.Identity): HRIO[Logger & Telemetry, Unit] =
      con.use { Q.insert(app).single }

    override def byId(id: M.App.Id): HRIO[Logger & Telemetry, M.App.Identity] =
      con.use { Q.selectById(id).single(s"Invalid app id '$id'") }

    override def byName(userId: M.User.Id, name: String): HRIO[Logger & Telemetry, Option[M.App.Identity]] =
      con.use { Q.byName(userId, name).option }

    override def selectAll(userId: M.User.Id): HRIO[Logger & Telemetry, Chunk[M.App.Identity]] =
      con.use { Q.byUserId(userId).chunk }

    // =====| Queries |=====

    private object Q extends TableQueries[M.App.Id, M.App] {

      val byUserId: QueryIO[M.User.Id, M.App.Identity] =
        Prepare.selectIO("App - byUserId") { Input[M.User.Id] } { userId =>
          Select
            .from[M.App]("a")
            .where { a => a.userId === userId }
            .returning { a => a }
        }

      val byName: QueryIO[(M.User.Id, String), M.App.Identity] =
        Prepare.selectIO("App - byName") { Input[M.User.Id] ~ Input[String] } { (userId, appName) =>
          Select
            .from[M.App]("a")
            .where { a => a.userId === userId && a.name === appName }
            .returning { a => a }
        }

    }

  }

}
