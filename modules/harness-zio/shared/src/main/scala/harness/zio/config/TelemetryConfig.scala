package harness.zio.config

import cats.syntax.either.*
import harness.zio.*
import zio.*
import zio.json.*

final case class TelemetryConfig(
    sources: List[TelemetryConfig.Src],
) {

  def telemetry: URIO[Scope, Telemetry] =
    ZIO.foreach(sources)(_.getSource).map {
      case head :: tail => tail.foldLeft(head)(_ && _)
      case Nil          => Telemetry.none
    }

}
object TelemetryConfig {

  final case class Src(getSource: URIO[Scope, Telemetry])

  def jsonDecoder(configDecoders: HConfig.KeyedConfigDecoder[Src]*): JsonDecoder[TelemetryConfig] = {
    implicit val sourceDecoder: JsonDecoder[List[Src]] =
      HConfig.KeyedConfig
        .makeMapDecoder[Src](configDecoders*)
    // .orElse(JsonDecoder.list(Config.KeyedConfig.makeDecoder(configDecoders*)))

    DeriveJsonDecoder.gen
  }

  def defaultJsonDecoder: JsonDecoder[TelemetryConfig] =
    jsonDecoder(
      loggedDecoder,
    )

  // =====|  |=====

  final case class LoggedConfig(
      level: Option[Logger.LogLevel],
  ) derives JsonCodec

  val loggedDecoder: HConfig.KeyedConfigDecoder[TelemetryConfig.Src] =
    HConfig.KeyedConfigDecoder.make[LoggedConfig, TelemetryConfig.Src]("logged") { config =>
      TelemetryConfig.Src(ZIO.succeed(Telemetry.log.withMinLogTolerance(config.level)))
    }

}
