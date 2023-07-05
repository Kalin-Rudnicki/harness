package harness.archive.client

import cats.syntax.option.*
import harness.cli.*
import harness.http.client.*
import harness.zio.*
import zio.*

object TestMain extends ExecutableApp {

  override val executable: Executable =
    Executable
      .withParser(Parser.unit)
      .withLayer {
        HttpClient.defaultLayer >>>
          ArchiveSpec.layer("my-app", "http://localhost:3001") >>>
          (ArchiveLoggerTarget.loggerLayerWithSource(Logger.LogLevel.Trace.some, None) ++
            ArchiveTelemetry.layer)
      }
      .withEffect {
        for {
          _ <- Logger.log.info("INFO", "round" -> 1)
          _ <- Logger.log.debug("DEBUG", "round" -> 1)
          _ <- Logger.log.trace("TRACE", "round" -> 1)
          _ <- Clock.sleep(10.seconds)
          _ <- Logger.log.info("INFO", "round" -> 2)
          _ <- Logger.log.debug("DEBUG", "round" -> 2)
          _ <- Logger.log.trace("TRACE", "round" -> 2)
          _ <- Clock.sleep(2.seconds).trace("waiting...", "round" -> 3)
        } yield ()
      }

}
