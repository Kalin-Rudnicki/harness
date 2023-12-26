package harness.archive.client

import cats.syntax.option.*
import harness.cli.*
import harness.http.client.*
import harness.zio.*
import zio.*

object TestMain extends ExecutableApp {

  override val config: ExecutableApp.Config = ExecutableApp.Config.default.addArchiveDecoders

  override val executable: Executable =
    Executable
      .withParser(Parser.unit)
      .withEffect {
        for {
          _ <- ZIO.unit.telemetrize("INFO", Logger.LogLevel.Info, "round" -> 1, "type" -> "trace")
          _ <- ZIO.unit.telemetrize("DEBUG", Logger.LogLevel.Debug, "round" -> 1, "type" -> "trace")
          _ <- ZIO.unit.telemetrize("TRACE", Logger.LogLevel.Trace, "round" -> 1, "type" -> "trace")
          _ <- Logger.log.info("INFO", "round" -> 1, "type" -> "log")
          _ <- Logger.log.debug("DEBUG", "round" -> 1, "type" -> "log")
          _ <- Logger.log.trace("TRACE", "round" -> 1, "type" -> "log")
          _ <- Clock.sleep(10.seconds)
          _ <- ZIO.unit.telemetrize("INFO", Logger.LogLevel.Info, "round" -> 2, "type" -> "trace")
          _ <- ZIO.unit.telemetrize("DEBUG", Logger.LogLevel.Debug, "round" -> 2, "type" -> "trace")
          _ <- ZIO.unit.telemetrize("TRACE", Logger.LogLevel.Trace, "round" -> 2, "type" -> "trace")
          _ <- Logger.log.info("INFO", "round" -> 2, "type" -> "log")
          _ <- Logger.log.debug("DEBUG", "round" -> 2, "type" -> "log")
          _ <- Logger.log.trace("TRACE", "round" -> 2, "type" -> "log")
          _ <- Clock.sleep(2.seconds).telemetrize("waiting...", "round" -> 3)
        } yield ()
      }

}
