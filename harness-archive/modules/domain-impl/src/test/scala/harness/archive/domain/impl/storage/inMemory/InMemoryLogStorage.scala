package harness.archive.domain.impl.storage.inMemory

import harness.archive.api.model as Api
import harness.archive.domain.model.*
import harness.archive.domain.storage.LogStorage
import harness.zio.*
import zio.*

// TODO (KR) : Create Contract + Spec
final case class InMemoryLogStorage(stateRef: Ref.Synchronized[DbState]) extends LogStorage {

  override def insertAll(logs: Chunk[Log]): ZIO[Logger & Telemetry, DomainError, Unit] =
    ZIO.foreachDiscard(logs) { log =>
      stateRef.updateZIO { state =>
        for {
          _ <- state.logs.verifyKeyDne(log.id)
          _ <- state.apps.get(log.appId)
          updatedLogs = state.logs.state.updated(log.id, log)
        } yield state.copy(logs = DbState.LogTable(updatedLogs))
      }
    }

  override def forAppId(appId: Api.app.AppId): ZIO[Logger & Telemetry, DomainError, Chunk[Log]] =
    stateRef.get.map { state => Chunk.fromIterable(state.logs.AppIndex.find(appId)) }

  override def deleteOutdated(now: Long): ZIO[Logger & Telemetry, DomainError, Int] =
    stateRef.modify { state =>
      val (outdated, updatedLogs) = state.logs.state.partition { case (_, log) => log.keepUntilEpochMS < now }
      val updatedState = state.copy(logs = DbState.LogTable(updatedLogs))
      (outdated.size, updatedState)
    }

  override def allForQuery(userId: Api.user.UserId, query: (App, Log) => Boolean): ZIO[Logger & Telemetry, DomainError, Chunk[Log]] =
    stateRef.get.flatMap { state =>
      ZIO.foreach(Chunk.fromIterable(state.logs.state.values)) { log => state.apps.get(log.appId).map((_, log)) }.map(_.filter(query(_, _)).map(_._2))
    }

}
object InMemoryLogStorage {

  val layer: URLayer[Ref.Synchronized[DbState], InMemoryLogStorage] =
    ZLayer.fromFunction { InMemoryLogStorage.apply }

}
