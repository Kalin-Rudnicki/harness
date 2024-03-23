package harness.console.error

import cats.data.NonEmptyList

sealed trait ExecuteError extends Throwable {

  override final def getMessage: String = this match {
    case ExecuteError.MissingCommand(options)      => s"Missing command, options:${options.toList.map(o => s"\n  - $o").mkString}"
    case ExecuteError.InvalidCommand(cmd, options) => s"Invalid command '$cmd', options:${options.toList.map(o => s"\n  - $o").mkString}"
  }

}
object ExecuteError {

  final case class MissingCommand(options: NonEmptyList[String]) extends ExecuteError
  final case class InvalidCommand(cmd: String, options: NonEmptyList[String]) extends ExecuteError

}
