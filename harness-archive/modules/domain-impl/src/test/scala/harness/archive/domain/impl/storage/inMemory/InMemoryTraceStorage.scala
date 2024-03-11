package harness.archive.domain.impl.storage.inMemory

import harness.archive.api.model as Api
import harness.archive.domain.model.*
import harness.archive.domain.storage.TraceStorage
import harness.zio.*
import zio.{Trace as _, *}

// TODO (KR) : Create Contract + Spec
final case class InMemoryTraceStorage(stateRef: Ref.Synchronized[DbState]) extends TraceStorage {

  override def insertAll(traces: Chunk[Trace]): ZIO[Logger & Telemetry, DomainError, Unit] =
    ZIO.foreachDiscard(traces) { trace =>
      stateRef.updateZIO { state =>
        for {
          _ <- state.traces.verifyKeyDne(trace.id)
          _ <- state.apps.get(trace.appId)
          updatedTraces = state.traces.state.updated(trace.id, trace)
        } yield state.copy(traces = DbState.TraceTable(updatedTraces))
      }
    }

  override def forAppId(appId: Api.app.AppId): ZIO[Logger & Telemetry, DomainError, Chunk[Trace]] =
    stateRef.get.map { state => Chunk.fromIterable(state.traces.AppIndex.find(appId)) }

  override def deleteOutdated(now: Long): ZIO[Logger & Telemetry, DomainError, Int] =
    stateRef.modify { state =>
      val (outdated, updatedTraces) = state.traces.state.partition { case (_, trace) => trace.keepUntilEpochMS < now }
      val updatedState = state.copy(traces = DbState.TraceTable(updatedTraces))
      (outdated.size, updatedState)
    }

  override def allForQuery(userId: Api.user.UserId, query: (App, Trace) => Boolean): ZIO[Logger & Telemetry, DomainError, Chunk[Trace]] =
    stateRef.get.flatMap { state =>
      ZIO.foreach(Chunk.fromIterable(state.traces.state.values)) { trace => state.apps.get(trace.appId).map((_, trace)) }.map(_.filter(query(_, _)).map(_._2))
    }

}
object InMemoryTraceStorage {

  val layer: URLayer[Ref.Synchronized[DbState], InMemoryTraceStorage] =
    ZLayer.fromFunction { InMemoryTraceStorage.apply }

}
