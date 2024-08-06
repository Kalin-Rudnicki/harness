package harness.sql

import cats.syntax.either.*
import zio.*
import zio.json.*

final case class DbConfig private (
    psqlJdbcUrl: String,
    raw: DbConfig.Raw,
) {
  def target: DbConfig.Target = raw.target
  def credentials: DbConfig.Credentials = raw.credentials
  def pool: DbConfig.PoolConfig = raw.pool
  def logging: DbConfig.Logging = raw.logging
}
object DbConfig {

  implicit val jsonCodec: JsonCodec[DbConfig] =
    JsonCodec[DbConfig.Raw].transformOrFail(
      _.toConfig,
      _.raw,
    )

  final case class PoolConfig(
      minConnections: Int,
      maxConnections: Int,
      duration: Duration,
  ) derives JsonCodec

  final case class Credentials(
      username: String,
      password: String,
  ) derives JsonCodec

  final case class Target(
      database: String,
      host: Option[String],
      port: Option[Int],
  ) derives JsonCodec

  final case class Logging(
      addConnectionContext: Boolean,
      addTransactionContext: Boolean,
  ) derives JsonCodec

  final case class Raw(
      target: Target,
      credentials: Credentials,
      pool: PoolConfig,
      logging: Logging,
  ) derives JsonCodec { raw =>

    def toConfig: Either[String, DbConfig] = {
      val database = raw.target.database

      ((raw.target.host, raw.target.port) match {
        case (Some(host), Some(port)) => s"//$host:$port/$database".asRight
        case (Some(host), None)       => s"//$host/$database".asRight
        case (None, None)             => database.asRight
        case (None, Some(_))          => "You must supply a host when supplying a port".asLeft
      }).map { psqlJdbcUrlSuffix =>
        val psqlJdbcUrl = s"jdbc:postgresql:$psqlJdbcUrlSuffix"
        DbConfig(psqlJdbcUrl, raw)
      }
    }

    def toConfigUnsafe: DbConfig =
      raw.toConfig.fold(err => throw new RuntimeException(s"Error creating db config: $err"), identity)

  }

}
