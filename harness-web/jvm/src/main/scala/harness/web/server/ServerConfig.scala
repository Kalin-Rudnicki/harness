package harness.web.server

import harness.cli.*

final case class ServerConfig(
    port: Option[Int],
    sslConfig: Option[ServerConfig.SslConfig],
    resDir: String,
    // TODO (KR) : Options relating to logging requests
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
        ).map(SslConfig.apply).optional &&
        Parser.value[String](LongName.unsafe("res-dir"), Defaultable.None).default("res", true)
    ).map(ServerConfig.apply)

}
