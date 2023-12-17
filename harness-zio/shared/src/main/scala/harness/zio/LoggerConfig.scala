package harness.zio

import cats.syntax.either.*
import cats.syntax.option.*
import harness.core.*
import zio.json.*
import zio.json.ast.*

final case class LoggerConfig(
    context: Map[String, String],
    sources: List[Logger.Source],
) {

  def logger: Logger =
    Logger(
      sources = sources,
      defaultMinLogTolerance = Logger.LogLevel.Never,
      defaultContext = context,
    )

}
object LoggerConfig {

  def jsonDecoder(configDecoders: Config.KeyedConfigDecoder[Logger.Source]*): JsonDecoder[LoggerConfig] = {
    implicit val sourceDecoder: JsonDecoder[List[Logger.Source]] =
      Config.KeyedConfig
        .makeMapDecoder[Logger.Source](configDecoders*)
    // .orElse(JsonDecoder.list(Config.KeyedConfig.makeDecoder(configDecoders*)))

    DeriveJsonDecoder.gen
  }

  // =====|  |=====

  implicit val colorModeJsonCodec: JsonCodec[ColorMode] =
    JsonCodec.fromHarnessStringEncoderAndDecoder

  object configs {

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

  val stdOutDecoder: Config.KeyedConfigDecoder[Logger.Source] =
    Config.KeyedConfigDecoder.make[configs.ToleranceAndColorMode, Logger.Source]("std-out") { config => Logger.Source.stdOut(config.logTolerance.some, config.colorMode).asRight }

  val stdOutJsonDecoder: Config.KeyedConfigDecoder[Logger.Source] =
    Config.KeyedConfigDecoder.make[configs.Tolerance, Logger.Source]("std-out-json") { config => Logger.Source.stdOutJson(config.logTolerance.some).asRight }

}
