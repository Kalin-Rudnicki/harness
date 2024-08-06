package harness.zio.config

import cats.syntax.either.*
import cats.syntax.option.*
import harness.core.*
import harness.zio.Logger
import harness.zio.json.*
import zio.json.*

object StdConfigs {

  implicit val colorModeJsonCodec: JsonCodec[ColorMode] =
    JsonCodec.fromHarnessStringEncoderAndDecoder

  final case class LogLevelOrDefault(optLevel: Option[Logger.LogLevel])
  object LogLevelOrDefault {

    implicit val jsonCodec: JsonCodec[LogLevelOrDefault] =
      JsonCodec.string.transformOrFail(
        _.toUpperCase match {
          case "DEFAULT" => LogLevelOrDefault(None).asRight
          case string    => JsonCodec[Logger.LogLevel].decodeJson(string).map(l => LogLevelOrDefault(l.some))
        },
        _.optLevel.fold("DEFAULT")(_.rawDisplayName),
      )

  }

}
