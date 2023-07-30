package harness.archive.api.service.storage

import harness.archive.api.db.model as M
import harness.sql.*
import harness.sql.query.{given, *}
import harness.zio.*
import zio.*

trait SessionStorage {
  def insert(session: M.Session.Identity): HRIO[Logger & Telemetry, Unit]
  def sessionFromSessionToken(token: String): HRIO[Logger & Telemetry, Option[M.Session.Identity]]
  def userFromSessionToken(token: String): HRIO[Logger & Telemetry, Option[M.User.Identity]]
  def deleteById(id: M.Session.Id): HRIO[Logger & Telemetry, Unit]
}
object SessionStorage {

  // =====| API |=====

  def insert(session: M.Session.Identity): HRIO[SessionStorage & Logger & Telemetry, Unit] =
    ZIO.serviceWithZIO[SessionStorage](_.insert(session))

  def sessionFromSessionToken(token: String): HRIO[SessionStorage & Logger & Telemetry, Option[M.Session.Identity]] =
    ZIO.serviceWithZIO[SessionStorage](_.sessionFromSessionToken(token))

  def userFromSessionToken(token: String): HRIO[SessionStorage & Logger & Telemetry, Option[M.User.Identity]] =
    ZIO.serviceWithZIO[SessionStorage](_.userFromSessionToken(token))

  def deleteById(id: M.Session.Id): HRIO[SessionStorage & Logger & Telemetry, Unit] =
    ZIO.serviceWithZIO[SessionStorage](_.deleteById(id))

  // =====| Live |=====

  val liveLayer: URLayer[JDBCConnection, SessionStorage] =
    ZLayer.fromFunction(new Live(_))

  final class Live(con: JDBCConnection) extends SessionStorage {

    override def insert(session: M.Session.Identity): HRIO[Logger & Telemetry, Unit] =
      con.use { Q.insert(session).single }

    override def sessionFromSessionToken(token: String): HRIO[Logger & Telemetry, Option[M.Session.Identity]] =
      con.use { Q.sessionFromSessionToken(token).option }

    override def userFromSessionToken(token: String): HRIO[Logger & Telemetry, Option[M.User.Identity]] =
      con.use { Q.userFromSessionToken(token).option }

    override def deleteById(id: M.Session.Id): HRIO[Logger & Telemetry, Unit] =
      con.use { Q.deleteById(id).single }

    // =====| Queries |=====

    private object Q extends TableQueries[M.Session.Id, M.Session] {

      val sessionFromSessionToken: QueryIO[String, M.Session.Identity] =
        Prepare.selectIO(s"Session - sessionFromSessionToken") { Input[String] } { token =>
          Select
            .from[M.Session]("s")
            .where { s => s.token === token }
            .returning { s => s }
        }

      val userFromSessionToken: QueryIO[String, M.User.Identity] =
        Prepare.selectIO("Session - userFromSessionToken") { Input[String] } { token =>
          Select
            .from[M.Session]("s")
            .join[M.User]("u")
            .on { case (s, u) => s.userId === u.id }
            .where { case (s, _) => s.token === token }
            .returning { case (_, u) => u }
        }

    }

  }

}
