package harness.sql

import harness.core.*
import harness.zio.*
import zio.*
import zio.json.*

final case class DbConfig private (
    psqlJdbcUrl: String,
    credentials: DbConfig.Credentials,
    pool: DbConfig.PoolConfig,
)
object DbConfig {

  final case class PoolConfig(
      minConnections: Int,
      maxConnections: Int,
      duration: Duration,
  )
  object PoolConfig {
    implicit val jsonCodec: JsonCodec[PoolConfig] = DeriveJsonCodec.gen
  }

  final case class Credentials(
      username: String,
      password: String,
  )
  object Credentials {
    implicit val jsonCodec: JsonCodec[Credentials] = DeriveJsonCodec.gen
  }

  private final case class Target(
      database: String,
      host: Option[String],
      port: Option[Int],
  )
  private object Target {
    implicit val jsonCodec: JsonCodec[Target] = DeriveJsonCodec.gen
  }

  private final case class Raw(
      target: Target,
      credentials: Credentials,
      pool: PoolConfig,
  )
  private object Raw {
    implicit val jsonCodec: JsonCodec[Raw] = DeriveJsonCodec.gen
  }

  def configLayer(configJsonPath: String*): HRLayer[Config, DbConfig] =
    ZLayer.fromZIO {
      for {
        raw <- Config.read[DbConfig.Raw](configJsonPath*)
        database = raw.target.database
        psqlJdbcUrlSuffix <- (raw.target.host, raw.target.port) match {
          case (Some(host), Some(port)) => ZIO.succeed(s"//$host:$port/$database")
          case (Some(host), None)       => ZIO.succeed(s"//$host/$database")
          case (None, None)             => ZIO.succeed(database)
          case (None, Some(_))          => ZIO.fail(HError.InternalDefect("You must supply a host when supplying a port"))
        }
        psqlJdbcUrl = s"jdbc:postgresql:$psqlJdbcUrlSuffix"
        // TODO (KR) : validations on config fields (don't allow stupid shit)
      } yield DbConfig(psqlJdbcUrl, raw.credentials, raw.pool)
    }
  def configLayer: HRLayer[Config, DbConfig] =
    configLayer("db")

}
