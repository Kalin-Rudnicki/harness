package harness.zio

import cats.syntax.either.*
import zio.json.*

final case class TelemetryConfig(
    sources: List[Telemetry],
) {

  def telemetry: Telemetry =
    sources.foldLeft(Telemetry.none)(_ && _)

}
object TelemetryConfig {

  def jsonDecoder(configDecoders: Config.KeyedConfigDecoder[Telemetry]*): JsonDecoder[TelemetryConfig] = {
    implicit val sourceDecoder: JsonDecoder[List[Telemetry]] =
      Config.KeyedConfig
        .makeMapDecoder[Telemetry](configDecoders*)
    // .orElse(JsonDecoder.list(Config.KeyedConfig.makeDecoder(configDecoders*)))

    DeriveJsonDecoder.gen
  }

  val loggedDecoder: Config.KeyedConfigDecoder[Telemetry] =
    Config.KeyedConfigDecoder.make[StdConfigs.Tolerance, Telemetry]("logged") { config => Telemetry.log.withMinLogTolerance(config.logTolerance).asRight }

}
