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
        HttpClient.defaultLayer.map { env =>
          ZEnvironment(
            Logger.default(
              sources = List(
                Logger.Source.const(new ArchiveTarget("my-app", "http://localhost:3001", env.get[HttpClient.ClientT]), Logger.LogLevel.Trace.some, None),
                Logger.Source.stdOut(None, None),
              ),
              defaultMinLogTolerance = Logger.LogLevel.Debug,
            ),
          )
        }
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
        } yield ()
      }

}
