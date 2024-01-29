package harness.http.server

import harness.cli.*
import harness.core.*
import harness.zio.*
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
      // keystore
      keystoreRef: String,
      keystorePassword: String,
      keystoreRefType: SslConfig.RefType,
      // truststore
      truststoreRef: String,
      truststorePassword: String,
      truststoreRefType: SslConfig.RefType,
  )
  object SslConfig {

    enum RefType extends Enum[RefType] { case Str, Jar, File }
    object RefType extends Enum.Companion[RefType] {
      implicit val jsonCodec: JsonCodec[RefType] = JsonCodec.fromHarnessStringEncoderAndDecoder
    }

    implicit val jsonCodec: JsonCodec[SslConfig] = DeriveJsonCodec.gen

  }

  implicit val jsonCodec: JsonCodec[ServerConfig] = DeriveJsonCodec.gen

}
