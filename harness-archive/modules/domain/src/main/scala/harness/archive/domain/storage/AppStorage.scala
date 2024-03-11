package harness.archive.domain.storage

import harness.archive.api.model as Api
import harness.archive.domain.model.*
import harness.zio.*
import zio.*

trait AppStorage {
  def insert(app: App): ZIO[Logger & Telemetry, DomainError, Unit]
  def byId(id: Api.app.AppId): ZIO[Logger & Telemetry, DomainError, App]
  def byName(userId: Api.user.UserId, name: String): ZIO[Logger & Telemetry, DomainError, Option[App]]
  def selectAll(userId: Api.user.UserId): ZIO[Logger & Telemetry, DomainError, Chunk[App]]
}
