package harness.zio.error

import cats.syntax.either.*
import harness.cli.FinalizedParser
import harness.zio.*
import zio.json.*

sealed trait ExecutableError[+E]
object ExecutableError {

  final case class External[+E](error: E) extends ExecutableError[E]

  sealed trait Internal extends ExecutableError[Nothing] with JsonShowable[Internal]
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
      case External(error)    => eErrorLogger.convert(error)
      case internal: Internal => (Logger.LogLevel.Fatal, internal.showJsonPretty) // TODO (KR) :
    }

}
