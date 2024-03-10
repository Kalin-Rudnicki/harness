package harness.archive.domain.impl.storage.postgres

import harness.sql.*
import harness.sql.query.{given, *}
import harness.zio.*
import harness.archive.api.model as Api
import harness.archive.db.model as Db
import harness.archive.domain.impl.storage.postgres.StorageUtils.*
import harness.archive.domain.model.DomainError
import harness.archive.domain.model as Domain
import harness.archive.domain.storage.SessionStorage
import zio.*

final case class LiveSessionStorage(con: JDBCConnection) extends SessionStorage {
  import LiveSessionStorage.Q

  override def insert(session: Domain.Session): ZIO[Logger & Telemetry, DomainError, Unit] =
    con.use { Q.insert(Db.Session.fromDomain(session)).single }.mapError(DomainError.UnexpectedStorageError(_))

  override def sessionFromSessionToken(token: Api.user.UserToken): ZIO[Logger & Telemetry, DomainError, Option[Domain.Session]] =
    con.use { Q.sessionFromSessionToken(token).option }.mapBoth(DomainError.UnexpectedStorageError(_), _.map(Db.Session.toDomain))

  override def userFromSessionToken(token: Api.user.UserToken): ZIO[Logger & Telemetry, DomainError, Option[Domain.User]] =
    con.use { Q.userFromSessionToken(token).option }.mapBoth(DomainError.UnexpectedStorageError(_), _.map(Db.User.toDomain))

  override def deleteById(id: Api.user.SessionId): ZIO[Logger & Telemetry, DomainError, Unit] =
    con.use { Q.deleteById(id).single }.mapError(DomainError.UnexpectedStorageError(_))

}
object LiveSessionStorage {

  val liveLayer: URLayer[JDBCConnection, SessionStorage] =
    ZLayer.fromFunction { LiveSessionStorage.apply }

  private object Q extends TableQueries[Db.Session.Id, Db.Session] {

    val sessionFromSessionToken: QueryIO[Api.user.UserToken, Db.Session.Identity] =
      Prepare.selectIO(s"Session - sessionFromSessionToken") { Input[Api.user.UserToken] } { token =>
        Select
          .from[Db.Session]("s")
          .where { s => s.token === token }
          .returning { s => s }
      }

    val userFromSessionToken: QueryIO[Api.user.UserToken, Db.User.Identity] =
      Prepare.selectIO("Session - userFromSessionToken") { Input[Api.user.UserToken] } { token =>
        Select
          .from[Db.Session]("s")
          .join[Db.User]("u")
          .on { case (s, u) => s.userId === u.id }
          .where { case (s, _) => s.token === token }
          .returning { case (_, u) => u }
      }

  }

}
