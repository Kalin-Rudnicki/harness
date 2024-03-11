package harness.archive.domain.impl.storage.postgres

import harness.archive.api.model as Api
import harness.archive.db.model as Db
import harness.archive.domain.impl.storage.postgres.StorageUtils.*
import harness.archive.domain.model.DomainError
import harness.archive.domain.model as Domain
import harness.archive.domain.storage.TraceStorage
import harness.sql.*
import harness.sql.query.{given, *}
import harness.zio.*
import zio.*

final case class LiveTraceStorage(con: JDBCConnection) extends TraceStorage {
  import LiveTraceStorage.Q

  override def insertAll(traces: Chunk[Domain.Trace]): ZIO[Logger & Telemetry, DomainError, Unit] =
    con.use { Q.insert.batched(traces.map(Db.Trace.fromDomain)).expectSize(traces.length) }.mapError(DomainError.UnexpectedStorageError(_))

  override def forAppId(appId: Api.app.AppId): ZIO[Logger & Telemetry, DomainError, Chunk[Domain.Trace]] =
    con.use { Q.forAppId(appId).chunk }.mapBoth(DomainError.UnexpectedStorageError(_), _.map(Db.Trace.toDomain))

  override def deleteOutdated(now: Long): ZIO[Logger & Telemetry, DomainError, Int] =
    con.use { Q.deleteOutdated(now).execute }.mapError(DomainError.UnexpectedStorageError(_))

  override def allForQuery(userId: Api.user.UserId, query: (Domain.App, Domain.Trace) => Boolean): ZIO[Logger & Telemetry, DomainError, Chunk[Domain.Trace]] =
    ZIO.scoped {
      con.use {
        Q.allWithApp(userId)
          .stream
          .map { case (app, trace) => (Db.App.toDomain(app), Db.Trace.toDomain(trace)) }
          .collect { case (app, trace) if query(app, trace) => trace }
          .runCollect
          .mapError(DomainError.UnexpectedStorageError(_))
      }
    }

}
object LiveTraceStorage {

  val liveLayer: URLayer[JDBCConnection, TraceStorage] =
    ZLayer.fromFunction { LiveTraceStorage.apply }

  private object Q extends TableQueries[Db.Trace.Id, Db.Trace] {

    val forAppId: QueryIO[Db.App.Id, Db.Trace.Identity] =
      Prepare.selectIO("Trace - forAppName") { Input[Db.App.Id] } { appId =>
        Select
          .from[Db.Trace]("t")
          .where { t => t.appId === appId }
          .returning { t => t }
      }

    val deleteOutdated: QueryI[Long] =
      Prepare.deleteI("Trace - deleteOutdated") { Input[Long] } { now =>
        Delete
          .from[Db.Trace]("t")
          .where { t => t.keepUntilEpochMS <= now }
      }

    val allWithApp: QueryIO[Db.User.Id, (Db.App.Identity, Db.Trace.Identity)] =
      Prepare.selectIO("Trace - allWithApp") { Input[Db.User.Id] } { userId =>
        Select
          .from[Db.App]("a")
          .join[Db.Trace]("t")
          .on { case (app, trace) => trace.appId === app.id }
          .where { case (app, _) => app.userId === userId }
          .returning { case (app, trace) => app ~ trace }
      }

  }

}
