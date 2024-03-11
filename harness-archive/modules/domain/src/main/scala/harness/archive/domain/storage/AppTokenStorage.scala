package harness.archive.domain.storage

import harness.archive.api.model as Api
import harness.archive.domain.model.*
import harness.zio.*
import zio.*

trait AppTokenStorage {
  def insert(session: AppToken): ZIO[Logger & Telemetry, DomainError, Unit]
  def fromToken(token: Api.app.AppToken): ZIO[Logger & Telemetry, DomainError, Option[AppToken]]
  def deleteById(id: Api.app.AppTokenId): ZIO[Logger & Telemetry, DomainError, Unit]
}
