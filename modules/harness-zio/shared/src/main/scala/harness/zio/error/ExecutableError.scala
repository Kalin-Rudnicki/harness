package harness.zio.error

import cats.syntax.either.*
import harness.cli.*
import harness.cli.error.BuildError
import harness.core.*
import harness.zio.*
import harness.zio.json.*

sealed trait ExecutableError
object ExecutableError {

  final case class CommandLineHelp(help: HelpMessage) extends ExecutableError
  final case class FailedToParseArgs(message: String) extends ExecutableError
  final case class FailedToLoadConfig(error: ConfigError.LoadError) extends ExecutableError
  final case class FailedToReadConfig(error: String) extends ExecutableError
  final case class MissingSubCommand(options: List[String]) extends ExecutableError
  final case class InvalidSubCommand(command: String, options: List[String]) extends ExecutableError
  final case class InvalidParser(fail: BuildError) extends ExecutableError
  final case class CommandLineParsingFailure(fail: HelpMessage, help: HelpMessage) extends ExecutableError
  final case class FailedToCreateService(service: String, cause: Throwable) extends ExecutableError

  // TODO (KR) : show child executables in message
  implicit val errorLogger: ErrorLogger[ExecutableError] =
    ErrorLogger.make[ExecutableError](
      {
        case _: CommandLineHelp           => Logger.LogLevel.Info
        case _: CommandLineParsingFailure => Logger.LogLevel.Fatal
        case _: MissingSubCommand         => Logger.LogLevel.Fatal
        case _: InvalidSubCommand         => Logger.LogLevel.Fatal
        case _: FailedToReadConfig        => Logger.LogLevel.Fatal
        case _: FailedToCreateService     => Logger.LogLevel.Fatal
        case _: FailedToParseArgs         => Logger.LogLevel.Fatal
        case _: FailedToLoadConfig        => Logger.LogLevel.Fatal
        case _: InvalidParser             => Logger.LogLevel.Fatal
      },
      { // TODO (KR) : improve this
        case CommandLineHelp(help)                 => help.toString
        case CommandLineParsingFailure(fail, _)    => s"Error parsing command line:\n\n$fail\n"
        case MissingSubCommand(options)            => s"Executable is missing sub-command, options:${options.map { o => s"\n  - $o" }.mkString}"
        case InvalidSubCommand(command, options)   => s"Executable received invalid sub-command '$command', options:${options.map { o => s"\n  - $o" }.mkString}"
        case FailedToReadConfig(error)             => s"Failed to read config: $error"
        case FailedToCreateService(service, cause) => s"Failed to create $service service: ${cause.safeGetMessage}"
        case FailedToParseArgs(message)            => s"Executable failed to parse args: $message"
        case FailedToLoadConfig(error)             => (error: Throwable).safeToJsonAST
        case InvalidParser(error)                  => (error: Throwable).safeToJsonAST
      },
    )

}
