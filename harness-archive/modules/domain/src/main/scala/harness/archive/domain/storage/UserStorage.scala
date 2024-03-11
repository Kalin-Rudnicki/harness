package harness.archive.domain.storage

import harness.archive.domain.model.*
import harness.zio.*
import zio.*

trait UserStorage {
  def insert(user: User): ZIO[Logger & Telemetry, DomainError, Unit]
  def byUsername(username: String): ZIO[Logger & Telemetry, DomainError, Option[User]]
}
