package harness.web.server

import harness.cli.*

final case class ServerConfig(
    port: Option[Int],
    // TODO (KR) : Options relating to logging requests
    // TODO (KR) : Option for where to serve page/favicon from
    sslConfig: Option[ServerConfig.SslConfig],
)
object ServerConfig {

  final case class SslConfig(
      keyPath: String,
      keyPassword: String,
  )

  val parser: Parser[ServerConfig] =
    (
      Parser.value[Int](LongName.unsafe("port")).optional &&
        (
          Parser.present(LongName.unsafe("https"), (), Defaultable.None, helpHint = List("Enable https/ssl")) &&
            Parser.value[String](LongName.unsafe("key-path"), Defaultable.None) &&
            Parser.value[String](LongName.unsafe("key-password"), Defaultable.None)
        ).map(SslConfig.apply).optional
    ).map(ServerConfig.apply)

}
