package harness.zio.error

import harness.zio.Sys

sealed trait SysError extends Throwable
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
