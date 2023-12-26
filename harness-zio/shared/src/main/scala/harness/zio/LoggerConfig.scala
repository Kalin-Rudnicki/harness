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

  def jsonDecoder(configDecoders: HConfig.KeyedConfigDecoder[Logger.Source]*): JsonDecoder[LoggerConfig] = {
    implicit val sourceDecoder: JsonDecoder[List[Logger.Source]] =
      HConfig.KeyedConfig
        .makeMapDecoder[Logger.Source](configDecoders*)
    // .orElse(JsonDecoder.list(Config.KeyedConfig.makeDecoder(configDecoders*)))

    DeriveJsonDecoder.gen
  }

  // =====|  |=====


  val stdOutDecoder: HConfig.KeyedConfigDecoder[Logger.Source] =
    HConfig.KeyedConfigDecoder.make[StdConfigs.ToleranceAndColorMode, Logger.Source]("std-out") { config => Logger.Source.stdOut(config.logTolerance.some, config.colorMode).asRight }

  val stdOutJsonDecoder: HConfig.KeyedConfigDecoder[Logger.Source] =
    HConfig.KeyedConfigDecoder.make[StdConfigs.Tolerance, Logger.Source]("std-out-json") { config => Logger.Source.stdOutJson(config.logTolerance.some).asRight }

}
