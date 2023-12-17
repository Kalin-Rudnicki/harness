package harness.zio

import zio.*

trait ExecutableApp extends ZIOAppDefault {

  val config: ExecutableApp.Config = ExecutableApp.Config.default

  val executable: Executable

  override def run: ZIO[ZIOAppArgs with Scope, Any, Any] =
    for {
      args <- ZIOAppArgs.getArgs
      exitCode <- executable(config, args.toList)
      _ <- exit(exitCode)
    } yield ()

}
object ExecutableApp {

  final case class Config(
      loggerDecoders: List[harness.zio.Config.KeyedConfigDecoder[Logger.Source]],
  ) { self =>

    def addLoggerDecoders(loggerDecoders: harness.zio.Config.KeyedConfigDecoder[Logger.Source]*): Config =
      self.copy(loggerDecoders = self.loggerDecoders ++ loggerDecoders)

  }
  object Config {

    val default: Config =
      Config(
        loggerDecoders = List(
          LoggerConfig.stdOutDecoder,
          LoggerConfig.stdOutJsonDecoder,
        ),
      )

  }

}
