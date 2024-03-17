package harness.zio.error

import cats.syntax.either.*
import harness.cli.FinalizedParser
import harness.cli.FinalizedParser.Result
import harness.core.*
import harness.zio.*
import zio.json.*

sealed trait ExecutableError[+E]
object ExecutableError {

  final case class External[+E](error: E) extends ExecutableError[E]

  sealed trait Internal extends ExecutableError[Nothing]
  object Internal {
    implicit val jsonEncoder: JsonEncoder[Internal] = {
      implicit val loadErrorEncoder: JsonEncoder[ConfigError.LoadError] = ConfigError.jsonCodec.encoder.narrow
      implicit val readErrorEncoder: JsonEncoder[ConfigError.ReadError] = ConfigError.jsonCodec.encoder.narrow
      implicit val nonSuccessEncoder: JsonEncoder[FinalizedParser.Result.NonSuccess] = JsonEncoder.string.contramap(_.toString)
      DeriveJsonEncoder.gen
    }
  }

  final case class FailedToParseIndexedArgs(message: String) extends ExecutableError.Internal
  final case class FailedToLoadConfig(error: ConfigError.LoadError) extends ExecutableError.Internal
  final case class FailedToReadConfig(error: ConfigError.ReadError) extends ExecutableError.Internal
  final case class MissingSubCommand(options: List[String]) extends ExecutableError.Internal
  final case class InvalidSubCommand(command: String, options: List[String]) extends ExecutableError.Internal
  final case class ParsingFailure(fail: FinalizedParser.Result.NonSuccess) extends ExecutableError.Internal

  implicit def errorLogger[E](implicit eErrorLogger: ErrorLogger[E]): ErrorLogger[ExecutableError[E]] =
    ErrorLogger.make[ExecutableError[E]] {
      case External(error) => eErrorLogger.convert(error)
      case internal: Internal =>
        val message: String = internal match {
          case FailedToParseIndexedArgs(message) =>
            s"Executable failed to parse args: $message"
          case FailedToLoadConfig(error) =>
            error.safeGetMessage
          case FailedToReadConfig(error) =>
            error.safeGetMessage
          case MissingSubCommand(options) =>
            s"Executable is missing sub-command, options:${options.map(o => s"\n  - $o").mkString}"
          case InvalidSubCommand(command, options) =>
            s"Executable received invalid sub-command '$command', options:${options.map(o => s"\n  - $o").mkString}"
          case ParsingFailure(fail) =>
            fail match {
              case Result.ParseFail(fail) => fail.toString // TODO (KR) : enough?
              case Result.InvalidArg(msg) => msg // TODO (KR) : enough?
              case Result.ParamNameConflict(duplicateParam) =>
                s"[Internal Defect] Unable to create CLI parser, param name conflict: ${duplicateParam.showName}"
              case Result.Help(_, _) =>
                "Should not be possible, failed with help message..."
            }
        }

        Logger.LogLevel.Fatal -> message
    }

}
