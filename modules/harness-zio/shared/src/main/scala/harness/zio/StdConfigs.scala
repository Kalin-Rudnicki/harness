package harness.zio

import harness.core.*
import harness.zio.json.*
import zio.json.*

object StdConfigs {

  implicit val colorModeJsonCodec: JsonCodec[ColorMode] =
    JsonCodec.fromHarnessStringEncoderAndDecoder

  final case class Tolerance(
      logTolerance: Logger.LogLevel,
  )
  object Tolerance {
    implicit val jsonCodec: JsonCodec[Tolerance] = DeriveJsonCodec.gen
  }

  final case class ToleranceAndColorMode(
      logTolerance: Logger.LogLevel,
      colorMode: ColorMode,
  )
  object ToleranceAndColorMode {
    implicit val jsonCodec: JsonCodec[ToleranceAndColorMode] = DeriveJsonCodec.gen
  }

}
