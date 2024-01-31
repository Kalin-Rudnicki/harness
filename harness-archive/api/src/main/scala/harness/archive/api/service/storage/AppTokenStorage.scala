package harness.archive.api.service.storage

import harness.archive.api.db.model as M
import harness.sql.*
import harness.sql.query.{given, *}
import harness.zio.*
import zio.*

trait AppTokenStorage {
  def insert(session: M.AppToken.Identity): HRIO[Logger & Telemetry, Unit]
  def fromToken(token: String): HRIO[Logger & Telemetry, Option[M.AppToken.Identity]]
  def deleteById(id: M.AppToken.Id): HRIO[Logger & Telemetry, Unit]
}
object AppTokenStorage {

  // =====| API |=====

  def insert(session: M.AppToken.Identity): HRIO[AppTokenStorage & Logger & Telemetry, Unit] =
    ZIO.serviceWithZIO[AppTokenStorage](_.insert(session))

  def fromToken(token: String): HRIO[AppTokenStorage & Logger & Telemetry, Option[M.AppToken.Identity]] =
    ZIO.serviceWithZIO[AppTokenStorage](_.fromToken(token))

  def deleteById(id: M.AppToken.Id): HRIO[AppTokenStorage & Logger & Telemetry, Unit] =
    ZIO.serviceWithZIO[AppTokenStorage](_.deleteById(id))

  // =====| Live |=====

  val liveLayer: URLayer[JDBCConnection, AppTokenStorage] =
    ZLayer.fromFunction(new Live(_))

  final class Live(con: JDBCConnection) extends AppTokenStorage {

    override def insert(session: M.AppToken.Identity): HRIO[Logger & Telemetry, Unit] =
      con.use { Q.insert(session).single }

    override def fromToken(token: String): HRIO[Logger & Telemetry, Option[M.AppToken.Identity]] =
      con.use { Q.fromToken(token).option }

    override def deleteById(id: M.AppToken.Id): HRIO[Logger & Telemetry, Unit] =
      con.use { Q.deleteById(id).single }

    // =====| Queries |=====

    private object Q extends TableQueries[M.AppToken.Id, M.AppToken] {

      val fromToken: QueryIO[String, M.AppToken.Identity] =
        Prepare.selectIO(s"AppToken - fromToken") { Input[String] } { token =>
          Select
            .from[M.AppToken]("at")
            .where { at => at.token === token }
            .returning { at => at }
        }

    }

  }

}
