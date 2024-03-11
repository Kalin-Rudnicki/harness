package harness.archive.domain.impl.storage.postgres

import harness.archive.api.model as Api
import harness.archive.db.model as Db
import harness.archive.domain.impl.storage.postgres.StorageUtils.*
import harness.archive.domain.model.DomainError
import harness.archive.domain.model as Domain
import harness.archive.domain.storage.AppTokenStorage
import harness.sql.*
import harness.sql.query.{given, *}
import harness.zio.*
import zio.*

final case class LiveAppTokenStorage(con: JDBCConnection) extends AppTokenStorage {
  import LiveAppTokenStorage.Q

  override def insert(session: Domain.AppToken): ZIO[Logger & Telemetry, DomainError, Unit] =
    con.use { Q.insert(Db.AppToken.fromDomain(session)).single }.mapError(DomainError.UnexpectedStorageError(_))

  override def fromToken(token: Api.app.AppToken): ZIO[Logger & Telemetry, DomainError, Option[Domain.AppToken]] =
    con.use { Q.fromToken(token).option }.mapBoth(DomainError.UnexpectedStorageError(_), _.map(Db.AppToken.toDomain))

  override def deleteById(id: Api.app.AppTokenId): ZIO[Logger & Telemetry, DomainError, Unit] =
    con.use { Q.deleteById(id).single }.mapError(DomainError.UnexpectedStorageError(_))

}
object LiveAppTokenStorage {

  val liveLayer: URLayer[JDBCConnection, AppTokenStorage] =
    ZLayer.fromFunction { LiveAppTokenStorage.apply }

  private object Q extends TableQueries[Db.AppToken.Id, Db.AppToken] {

    val fromToken: QueryIO[Api.app.AppToken, Db.AppToken.Identity] =
      Prepare.selectIO(s"AppToken - fromToken") { Input[Api.app.AppToken] } { token =>
        Select
          .from[Db.AppToken]("at")
          .where { at => at.token === token }
          .returning { at => at }
      }

  }

}
