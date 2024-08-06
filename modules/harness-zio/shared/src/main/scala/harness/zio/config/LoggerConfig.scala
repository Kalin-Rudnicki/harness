package harness.zio.config

import StdConfigs.*
import cats.syntax.either.*
import harness.core.*
import harness.zio.*
import zio.*
import zio.json.*

final case class LoggerConfig(
    context: Option[Map[String, String]],
    sources: List[LoggerConfig.Src],
    forwardToZio: Option[Boolean],
) {

  def logger: URIO[Scope, Logger] =
    ZIO.foreach(sources)(_.getSource).map { sources =>
      Logger(
        sources = Chunk.fromIterable(sources),
        context = Chunk.fromIterable(context.getOrElse(Map.empty)).sortBy(_._1),
        defaultMinLogTolerance = Logger.LogLevel.Never,
        forwardToZio = forwardToZio.getOrElse(false),
      )
    }

}
object LoggerConfig {

  final case class Src(getSource: URIO[Scope, Logger.Source])

  def jsonDecoder(configDecoders: HConfig.KeyedConfigDecoder[Src]*): JsonDecoder[LoggerConfig] = {
    implicit val sourceDecoder: JsonDecoder[List[Src]] =
      HConfig.KeyedConfig
        .makeMapDecoder[Src](configDecoders*)
    // .orElse(JsonDecoder.list(Config.KeyedConfig.makeDecoder(configDecoders*)))

    DeriveJsonDecoder.gen
  }

  def defaultJsonDecoder: JsonDecoder[LoggerConfig] =
    jsonDecoder(
      stdOutDecoder,
      stdOutJsonDecoder,
    )

  // =====|  |=====

  final case class StdOutConfig(
      level: LogLevelOrDefault,
      colorMode: ColorMode,
      logTimestamp: Boolean,
      logTrace: Boolean,
      logStack: Boolean,
  ) derives JsonCodec

  final case class StdOutJsonConfig(
      level: LogLevelOrDefault,
  ) derives JsonCodec

  val stdOutDecoder: HConfig.KeyedConfigDecoder[LoggerConfig.Src] =
    HConfig.KeyedConfigDecoder.makeEither[StdOutConfig, LoggerConfig.Src](Logger.Source.names.stdOut) { config =>
      LoggerConfig.Src(ZIO.succeed(Logger.Source.stdOut(config.level.optLevel, config.colorMode, config.logTimestamp, config.logTrace, config.logStack))).asRight
    }

  val stdOutJsonDecoder: HConfig.KeyedConfigDecoder[LoggerConfig.Src] =
    HConfig.KeyedConfigDecoder.makeEither[StdOutJsonConfig, LoggerConfig.Src](Logger.Source.names.stdOutJson) { config =>
      LoggerConfig.Src(ZIO.succeed(Logger.Source.stdOutJson(config.level.optLevel))).asRight
    }

}
