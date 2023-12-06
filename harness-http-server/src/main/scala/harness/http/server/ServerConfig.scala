package harness.http.server

import harness.cli.*
import zio.json.*

final case class ServerConfig(
    port: Option[Int],
    resDir: String,
    useJarResource: Boolean,
    ssl: Option[ServerConfig.SslConfig],
    // TODO (KR) : Options relating to logging requests
)
object ServerConfig {

  final case class SslConfig(
      keyPath: String,
      keyPassword: String,
  )
  object SslConfig {
    implicit val jsonCodec: JsonCodec[SslConfig] = DeriveJsonCodec.gen
  }

  implicit val jsonCodec: JsonCodec[ServerConfig] = DeriveJsonCodec.gen

}
