package harness.archive.domain.impl.storage.postgres

import harness.archive.api.model as Api
import harness.archive.db.model as Db
import harness.archive.domain.impl.storage.postgres.StorageUtils.*
import harness.archive.domain.model.DomainError
import harness.archive.domain.model as Domain
import harness.archive.domain.storage.LogStorage
import harness.sql.*
import harness.sql.query.{given, *}
import harness.zio.*
import zio.*

final case class LiveLogStorage(con: JDBCConnection) extends LogStorage {
  import LiveLogStorage.Q

  override def insertAll(logs: Chunk[Domain.Log]): ZIO[Logger & Telemetry, DomainError, Unit] =
    con.use { Q.insert.batched(logs.map(Db.Log.fromDomain)).expectSize(logs.length) }.mapError(DomainError.UnexpectedStorageError(_))

  override def forAppId(appId: Api.app.AppId): ZIO[Logger & Telemetry, DomainError, Chunk[Domain.Log]] =
    con.use { Q.forAppId(appId).chunk }.mapBoth(DomainError.UnexpectedStorageError(_), _.map(Db.Log.toDomain))

  override def deleteOutdated(now: Long): ZIO[Logger & Telemetry, DomainError, Int] =
    con.use { Q.deleteOutdated(now).execute }.mapError(DomainError.UnexpectedStorageError(_))

  override def allForQuery(userId: Api.user.UserId, query: (Domain.App, Domain.Log) => Boolean): ZIO[Logger & Telemetry, DomainError, Chunk[Domain.Log]] =
    ZIO.scoped {
      con.use {
        Q.allWithApp(userId)
          .stream
          .map { case (app, log) => (Db.App.toDomain(app), Db.Log.toDomain(log)) }
          .collect { case (app, log) if query(app, log) => log }
          .runCollect
          .mapError(DomainError.UnexpectedStorageError(_))
      }
    }

}
object LiveLogStorage {

  val liveLayer: URLayer[JDBCConnection, LogStorage] =
    ZLayer.fromFunction { LiveLogStorage.apply }

  private object Q extends TableQueries[Db.Log.Id, Db.Log] {

    val forAppId: QueryIO[Db.App.Id, Db.Log.Identity] =
      Prepare.selectIO("Log - byAppId") { Input[Db.App.Id] } { appId =>
        Select
          .from[Db.Log]("l")
          .where { l => l.appId === appId }
          .returning { l => l }
      }

    val deleteOutdated: QueryI[Long] =
      Prepare.deleteI("Log - deleteOutdated") { Input[Long] } { now =>
        Delete
          .from[Db.Log]("l")
          .where { l => l.keepUntilEpochMS <= now }
      }

    val allWithApp: QueryIO[Db.User.Id, (Db.App.Identity, Db.Log.Identity)] =
      Prepare.selectIO("Log - allWithApp") { Input[Db.User.Id] } { userId =>
        Select
          .from[Db.App]("a")
          .join[Db.Log]("l")
          .on { case (app, log) => log.appId === app.id }
          .where { case (app, _) => app.userId === userId }
          .returning { case (app, log) => app ~ log }
      }

  }

}
