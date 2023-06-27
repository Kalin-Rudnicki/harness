package harness.http.server

import harness.cli.*

final case class ServerConfig(
    port: Option[Int],
    resDir: String,
    useJarResource: Boolean,
    sslConfig: Option[ServerConfig.SslConfig],
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
        Parser.value[String](LongName.unsafe("res-dir"), Defaultable.None) &&
        Parser.helpMessageSection("[ Where to pull resources from ]", false) {
          Parser.present(LongName.unsafe("use-jar-resource"), true, Defaultable.None, helpHint = List("search for resources in jar")).indentedHelpMessage <||
            Parser.present(LongName.unsafe("use-fs-resource"), false, Defaultable.None, helpHint = List("search for resources in file-system")).indentedHelpMessage
        } &&
        Parser.helpMessageSection("[ SSL opts ]", false) {
          (
            Parser.present(LongName.unsafe("https"), (), Defaultable.None, helpHint = List("Enable https/ssl")) &&
              Parser.value[String](LongName.unsafe("key-path"), Defaultable.None) &&
              Parser.value[String](LongName.unsafe("key-password"), Defaultable.None)
          ).map(SslConfig.apply).optional
        }
    ).map(ServerConfig.apply)

}
