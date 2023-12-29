package harness.zio

import zio.*

trait ExecutableApp extends ZIOAppDefault {

  val config: ExecutableApp.Config = ExecutableApp.Config.default

  val executable: Executable

  override final def run: ZIO[ZIOAppArgs with Scope, Any, Any] =
    for {
      args <- ZIOAppArgs.getArgs
      exitCode <- executable(config, args.toList)
      _ <- exit(exitCode)
    } yield ()

}
object ExecutableApp {

  final case class Config(
      loggerDecoders: List[HConfig.KeyedConfigDecoder[LoggerConfig.Src]],
      telemetryDecoders: List[HConfig.KeyedConfigDecoder[TelemetryConfig.Src]],
  ) { self =>

    def addLoggerDecoders(loggerDecoders: HConfig.KeyedConfigDecoder[LoggerConfig.Src]*): Config =
      self.copy(loggerDecoders = self.loggerDecoders ++ loggerDecoders)

    def addTelemetryDecoders(telemetryDecoders: HConfig.KeyedConfigDecoder[TelemetryConfig.Src]*): Config =
      self.copy(telemetryDecoders = self.telemetryDecoders ++ telemetryDecoders)

  }
  object Config {

    val default: Config =
      Config(
        loggerDecoders = List(
          LoggerConfig.stdOutDecoder,
          LoggerConfig.stdOutJsonDecoder,
        ),
        telemetryDecoders = List(
          TelemetryConfig.loggedDecoder,
        ),
      )

  }

}
