package harness.web

import zio.json.*

trait HasStdClientConfig {
  val stdClientConfig: StdClientConfig
}
object HasStdClientConfig {

  final case class Basic(
      stdClientConfig: StdClientConfig,
  ) extends HasStdClientConfig
  object Basic {
    implicit val jsonCodec: JsonCodec[Basic] = DeriveJsonCodec.gen
  }

}
