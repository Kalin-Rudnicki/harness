package template.api.service.storage

import harness.sql.*
import harness.sql.query.{given, *}
import harness.zio.*
import template.api.db.model as M
import zio.*

trait SessionStorage {
  def insert(session: M.Session.Identity): HRIO[JDBCConnection & Logger & Telemetry, Unit]
  def sessionFromSessionToken(token: String): HRIO[JDBCConnection & Logger & Telemetry, Option[M.Session.Identity]]
  def userFromSessionToken(token: String): HRIO[JDBCConnection & Logger & Telemetry, Option[M.User.Identity]]
  def deleteById(id: M.Session.Id): HRIO[JDBCConnection & Logger & Telemetry, Unit]
}
object SessionStorage {

  // =====| API |=====

  def insert(session: M.Session.Identity): HRIO[SessionStorage & JDBCConnection & Logger & Telemetry, Unit] =
    ZIO.serviceWithZIO[SessionStorage](_.insert(session))

  def sessionFromSessionToken(token: String): HRIO[SessionStorage & JDBCConnection & Logger & Telemetry, Option[M.Session.Identity]] =
    ZIO.serviceWithZIO[SessionStorage](_.sessionFromSessionToken(token))

  def userFromSessionToken(token: String): HRIO[SessionStorage & JDBCConnection & Logger & Telemetry, Option[M.User.Identity]] =
    ZIO.serviceWithZIO[SessionStorage](_.userFromSessionToken(token))

  def deleteById(id: M.Session.Id): HRIO[SessionStorage & JDBCConnection & Logger & Telemetry, Unit] =
    ZIO.serviceWithZIO[SessionStorage](_.deleteById(id))

  // =====| Live |=====

  val liveLayer: ULayer[SessionStorage] = ZLayer.succeed(new Live)

  final class Live extends SessionStorage {

    override def insert(session: M.Session.Identity): HRIO[JDBCConnection & Logger & Telemetry, Unit] =
      Q.insert(session).single

    override def sessionFromSessionToken(token: String): HRIO[JDBCConnection & Logger & Telemetry, Option[M.Session.Identity]] =
      Q.sessionFromSessionToken(token).option

    override def userFromSessionToken(token: String): HRIO[JDBCConnection & Logger & Telemetry, Option[M.User.Identity]] =
      Q.userFromSessionToken(token).option

    override def deleteById(id: M.Session.Id): HRIO[JDBCConnection & Logger & Telemetry, Unit] =
      Q.deleteById(id).single

    // =====|  |=====

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
