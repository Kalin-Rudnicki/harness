package harness.zio.error

import harness.core.*
import harness.zio.Sys

sealed trait SysError extends Throwable {

  override final def getMessage: String = {
    def showCommandAndArgs(command: String, args: List[String]): String =
      args match {
        case Nil =>
          s"\n  command: $command"
        case _ =>
          s"\n  command: $command\n  args:${args.map(a => s"\n    - $a").mkString}"
      }

    this match {
      case SysError.NonZeroExitCode(command, args, exitCode) =>
        s"Executing sys-call yielded non-0 result ($exitCode)${showCommandAndArgs(command, args)}"
      case SysError.GenericError(command, args, cause) =>
        s"Encountered generic error executing sys-call${showCommandAndArgs(command, args)}\n  cause: ${cause.safeGetMessage}"
    }
  }

}
object SysError {

  final case class NonZeroExitCode(
      command: String,
      args: List[String],
      exitCode: Int,
  ) extends SysError
  object NonZeroExitCode {
    def apply(cmd: Sys.Command, exitCode: Int): NonZeroExitCode =
      NonZeroExitCode(cmd.cmdAndArgs.head, cmd.cmdAndArgs.tail, exitCode)
  }

  final case class GenericError(
      command: String,
      args: List[String],
      cause: Throwable,
  ) extends SysError
  object GenericError {
    def apply(cmd: Sys.Command, cause: Throwable): GenericError =
      GenericError(cmd.cmdAndArgs.head, cmd.cmdAndArgs.tail, cause)
  }

}
