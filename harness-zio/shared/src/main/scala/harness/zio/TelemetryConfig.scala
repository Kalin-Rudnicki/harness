package harness.zio

import cats.syntax.either.*
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

  val loggedDecoder: HConfig.KeyedConfigDecoder[TelemetryConfig.Src] =
    HConfig.KeyedConfigDecoder.make[StdConfigs.Tolerance, TelemetryConfig.Src]("logged") { config =>
      TelemetryConfig.Src(ZIO.succeed(Telemetry.log.withMinLogTolerance(config.logTolerance)))
    }

}
