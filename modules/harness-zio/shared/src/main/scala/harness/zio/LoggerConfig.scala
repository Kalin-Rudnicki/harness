package harness.zio

import cats.syntax.either.*
import cats.syntax.option.*
import zio.*
import zio.json.*

final case class LoggerConfig(
    context: Map[String, String],
    sources: List[LoggerConfig.Src],
) {

  def logger: URIO[Scope, Logger] =
    ZIO.foreach(sources)(_.getSource).map { sources =>
      Logger(
        sources = sources,
        defaultMinLogTolerance = Logger.LogLevel.Never,
        defaultContext = context,
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

  // =====|  |=====

  val stdOutDecoder: HConfig.KeyedConfigDecoder[LoggerConfig.Src] =
    HConfig.KeyedConfigDecoder.makeEither[StdConfigs.ToleranceAndColorMode, LoggerConfig.Src]("std-out") { config =>
      LoggerConfig.Src(ZIO.succeed(Logger.Source.stdOut(config.logTolerance.some, config.colorMode))).asRight
    }

  val stdOutJsonDecoder: HConfig.KeyedConfigDecoder[LoggerConfig.Src] =
    HConfig.KeyedConfigDecoder.makeEither[StdConfigs.Tolerance, LoggerConfig.Src]("std-out-json") { config =>
      LoggerConfig.Src(ZIO.succeed(Logger.Source.stdOutJson(config.logTolerance.some))).asRight
    }

}
