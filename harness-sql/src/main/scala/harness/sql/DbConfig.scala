package harness.sql

import cats.syntax.either.*
import harness.zio.*
import zio.*
import zio.json.*

final class DbConfig private (
    val psqlJdbcUrl: String,
    private val raw: DbConfig.Raw,
) {
  def target: DbConfig.Target = raw.target
  def credentials: DbConfig.Credentials = raw.credentials
  def pool: DbConfig.PoolConfig = raw.pool
}
object DbConfig {

  implicit val jsonCodec: JsonCodec[DbConfig] =
    DbConfig.Raw.jsonCodec.transformOrFail(
      { raw =>
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
      },
      _.raw,
    )

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

  final case class Target(
      database: String,
      host: Option[String],
      port: Option[Int],
  )
  object Target {
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

  def configLayer: HRLayer[HConfig, DbConfig] =
    HConfig.readLayer[DbConfig]("db")

}
