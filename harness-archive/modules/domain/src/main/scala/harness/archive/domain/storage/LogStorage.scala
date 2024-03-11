package harness.archive.domain.storage

import harness.archive.api.model as Api
import harness.archive.domain.model.*
import harness.zio.*
import zio.*

trait LogStorage {
  def insertAll(logs: Chunk[Log]): ZIO[Logger & Telemetry, DomainError, Unit]
  def forAppId(appId: Api.app.AppId): ZIO[Logger & Telemetry, DomainError, Chunk[Log]]
  def deleteOutdated(now: Long): ZIO[Logger & Telemetry, DomainError, Int]
  def allForQuery(userId: Api.user.UserId, query: (App, Log) => Boolean): ZIO[Logger & Telemetry, DomainError, Chunk[Log]]
}
