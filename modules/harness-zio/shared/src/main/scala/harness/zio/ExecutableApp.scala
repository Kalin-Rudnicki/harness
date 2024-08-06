package harness.zio

import harness.core.*
import zio.*

trait ExecutableApp extends ZIOAppDefault {

  val executable: Executable

  // TODO (KR) : Add support for different config loaders

  override final def run: URIO[ZIOAppArgs & Scope, Any] =
    for {
      _ <- Logger
        .withLogger(
          Logger(
            sources = Chunk(Logger.Source.stdOut(None, ColorMode.Extended, true, true, true)),
            context = Chunk.empty,
            defaultMinLogTolerance = Logger.LogLevel.Info,
            forwardToZio = false,
          ),
        )
        .set
      args <- ZIOAppArgs.getArgs
      exitCode <- executable(args.toList)
      _ <- exit(exitCode)
    } yield ()

}

object ExeTmpMain extends ExecutableApp {

  import harness.cli.*
  import zio.json.ast.Json

  final case class Person(
      firstName: String,
      lastName: String,
  )
  object Person {

    val parser: Params[Person] =
      (
        Params.value[String]("first-name") &&
          Params.value[String]("last-name")
      ).map { Person.apply }

  }

  private val sayHello: Executable =
    Executable
      .withConfig[Json]
      .withCommandLine(Person.parser)
      .implementJC { (json, person) =>
        Logger.log.info(s"Hello, ${person.firstName} ${person.lastName}") *>
          Logger.log.info(s"config:\n$json")
      }

  private val sayGoodbye: Executable =
    Executable.implement {
      Logger.log.info("Goodbye")
    }

  override val executable: Executable =
    Executable.fromSubCommands(
      "hi" -> sayHello,
      "bye" -> sayGoodbye,
    )

}
