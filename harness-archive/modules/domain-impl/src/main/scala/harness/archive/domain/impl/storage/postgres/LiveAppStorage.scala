package harness.archive.domain.impl.storage.postgres

import harness.archive.api.model as Api
import harness.archive.db.model as Db
import harness.archive.domain.impl.storage.postgres.StorageUtils.*
import harness.archive.domain.model.DomainError
import harness.archive.domain.model as Domain
import harness.archive.domain.storage.AppStorage
import harness.sql.*
import harness.sql.query.{given, *}
import harness.zio.*
import zio.*

final case class LiveAppStorage(con: JDBCConnection) extends AppStorage {
  import LiveAppStorage.Q

  override def insert(app: Domain.App): ZIO[Logger & Telemetry, DomainError, Unit] =
    con.use { Q.insert(Db.App.fromDomain(app)).single }.mapError(DomainError.UnexpectedStorageError(_))

  override def byId(id: Api.app.AppId): ZIO[Logger & Telemetry, DomainError, Domain.App] =
    con.use { Q.selectById(id).single[DomainError](DomainError.MissingExpectedInStorage(id.toString)) }.map(Db.App.toDomain)

  override def byName(userId: Api.user.UserId, name: String): ZIO[Logger & Telemetry, DomainError, Option[Domain.App]] =
    con.use { Q.byName(userId, name).option }.mapBoth(DomainError.UnexpectedStorageError(_), _.map(Db.App.toDomain))

  override def selectAll(userId: Api.user.UserId): ZIO[Logger & Telemetry, DomainError, Chunk[Domain.App]] =
    con.use { Q.byUserId(userId).chunk }.mapBoth(DomainError.UnexpectedStorageError(_), _.map(Db.App.toDomain))

}
object LiveAppStorage {

  val liveLayer: URLayer[JDBCConnection, AppStorage] =
    ZLayer.fromFunction { LiveAppStorage.apply }

  private object Q extends TableQueries[Db.App.Id, Db.App] {

    val byUserId: QueryIO[Db.User.Id, Db.App.Identity] =
      Prepare.selectIO("App - byUserId") { Input[Db.User.Id] } { userId =>
        Select
          .from[Db.App]("a")
          .where { a => a.userId === userId }
          .returning { a => a }
      }

    val byName: QueryIO[(Db.User.Id, String), Db.App.Identity] =
      Prepare.selectIO("App - byName") { Input[Db.User.Id] ~ Input[String] } { (userId, appName) =>
        Select
          .from[Db.App]("a")
          .where { a => a.userId === userId && a.name === appName }
          .returning { a => a }
      }

  }

}
