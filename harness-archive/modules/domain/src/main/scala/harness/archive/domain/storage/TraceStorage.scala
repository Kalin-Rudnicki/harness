package harness.archive.domain.storage

import harness.archive.api.model as Api
import harness.archive.domain.model.*
import harness.zio.*
import zio.{Trace as _, *}

trait TraceStorage {
  def insertAll(traces: Chunk[Trace]): ZIO[Logger & Telemetry, DomainError, Unit]
  def forAppId(appId: Api.app.AppId): ZIO[Logger & Telemetry, DomainError, Chunk[Trace]]
  def deleteOutdated(now: Long): ZIO[Logger & Telemetry, DomainError, Int]
  def allForQuery(userId: Api.user.UserId, query: (App, Trace) => Boolean): ZIO[Logger & Telemetry, DomainError, Chunk[Trace]]
}
