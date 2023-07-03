package template.api.service.storage

import harness.sql.*
import harness.sql.query.{given, *}
import harness.zio.*
import template.api.db.model.User
import template.api.db.model as M
import zio.*

trait UserStorage {
  def insert(user: M.User.Identity): HRIO[JDBCConnection & Logger & Telemetry, Unit]
  def byUsername(username: String): HRIO[JDBCConnection & Logger & Telemetry, Option[M.User.Identity]]
}
object UserStorage {

  // =====| API |=====

  def insert(user: M.User.Identity): HRIO[UserStorage & JDBCConnection & Logger & Telemetry, Unit] =
    ZIO.serviceWithZIO[UserStorage](_.insert(user))
  def byUsername(username: String): HRIO[UserStorage & JDBCConnection & Logger & Telemetry, Option[M.User.Identity]] =
    ZIO.serviceWithZIO[UserStorage](_.byUsername(username))

  // =====| Live |=====

  val liveLayer: ULayer[UserStorage] = ZLayer.succeed(new Live)
  
  final class Live extends UserStorage {

    override def insert(user: M.User.Identity): HRIO[JDBCConnection & Logger & Telemetry, Unit] =
      Q.insert(user).single

    override def byUsername(username: String): HRIO[JDBCConnection & Logger & Telemetry, Option[M.User.Identity]] =
      Q.byUsername(username).option

    // =====|  |=====

    private object Q extends TableQueries[M.User.Id, M.User] {

      val byUsername: QueryIO[String, M.User.Identity] =
        Prepare
          .selectIO("User - byUsername") {
            Input[String]
          } { username =>
            Select
              .from[M.User]("u")
              .where { u => u.lowerUsername === username }
              .returning { u => u }
          }
          .cmap[String](_.toLowerCase)

    }

  }

}
