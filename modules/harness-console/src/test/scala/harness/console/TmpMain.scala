package harness.console

import harness.zio.*
import zio.*

object TmpMain extends ExecutableApp {

  private val test: ConsoleProgram[Any, Unit, Unit] =
    ConsoleProgram.command("test").noParser.implement[Unit, Unit] { (_, _, _) =>
      Logger.log.info("Hello")
    }

  override val executable: Executable =
    ConsoleProgram
      .oneOf("outer")(
        test,
      )
      .toExecutable(
        ZLayer.empty,
        ZIO.succeed(((), ())),
      )

}
