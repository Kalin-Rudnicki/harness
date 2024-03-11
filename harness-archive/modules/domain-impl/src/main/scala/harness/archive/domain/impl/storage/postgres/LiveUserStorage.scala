package harness.archive.domain.impl.storage.postgres

import harness.archive.db.model as Db
import harness.archive.domain.model.DomainError
import harness.archive.domain.model as Domain
import harness.archive.domain.storage.UserStorage
import harness.sql.*
import harness.sql.query.{given, *}
import harness.zio.*
import zio.*

final case class LiveUserStorage(con: JDBCConnection) extends UserStorage {
  import LiveUserStorage.Q

  override def insert(user: Domain.User): ZIO[Logger & Telemetry, DomainError, Unit] =
    con.use { Q.insert(Db.User.fromDomain(user)).single }.mapError(DomainError.UnexpectedStorageError(_))

  override def byUsername(username: String): ZIO[Logger & Telemetry, DomainError, Option[Domain.User]] =
    con.use { Q.byUsername(username).option }.mapBoth(DomainError.UnexpectedStorageError(_), _.map(Db.User.toDomain))

}
object LiveUserStorage {

  val liveLayer: URLayer[JDBCConnection, UserStorage] =
    ZLayer.fromFunction { LiveUserStorage.apply }

  private object Q extends TableQueries[Db.User.Id, Db.User] {

    val byUsername: QueryIO[String, Db.User.Identity] =
      Prepare
        .selectIO("User - byUsername") { Input[String] } { username =>
          Select
            .from[Db.User]("u")
            .where { u => u.lowerUsername === username }
            .returning { u => u }
        }
        .cmap[String](_.toLowerCase)

  }

}
