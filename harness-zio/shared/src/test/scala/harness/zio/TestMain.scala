package harness.zio

import harness.cli.Parser
import zio.*

object TestMain extends ExecutableApp {

  override val executable: Executable =
    Executable.withParser(Parser.unit).withEffect {
      for {
        _ <- Logger.log.info("=====| TestMain |=====")
        _ <- ZIO.unit.trace("effect-1")
        _ <- Clock.sleep(Duration.fromSeconds(2)).trace("effect-2", Logger.LogLevel.Important, "effect-type" -> "query")
        _ <- Clock.sleep(Duration.fromNanos(2500000)).trace("effect-3")
        _ <- ZIO.fail("").trace("effect-4", Logger.LogLevel.Debug, "should-pass" -> false).either
        _ <- Logger.log.info(java.time.Duration.ofHours(1).toNanos)
        _ <- ZIO.acquireRelease(ZIO.unit) { _ => Logger.log.info("CLOSE") }
        _ <- Clock.sleep(1.minute)
      } yield ()
    }

}
