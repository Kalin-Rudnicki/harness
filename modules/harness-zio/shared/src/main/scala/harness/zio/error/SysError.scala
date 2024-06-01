package harness.zio.error

import harness.core.*
import harness.zio.Sys

sealed trait SysError extends Throwable {

  val cmd: Sys.Command

  private def showCommandAndArgs: String =
    cmd.args match
      case Nil  => s"\n  command: ${cmd.cmd}"
      case args => s"\n  command: ${cmd.cmd}\n  args:${args.map(a => s"\n    - $a").mkString}"

  override final def getMessage: String = {

    this match {
      case SysError.NonZeroExitCode(cmd, exitCode) =>
        s"Executing sys-call yielded non-0 result ($exitCode)$showCommandAndArgs"
      case SysError.GenericError(cmd, cause) =>
        s"Encountered generic error executing sys-call$showCommandAndArgs\n  cause: ${cause.safeGetMessage}"
    }
  }

}
object SysError {

  final case class NonZeroExitCode(
                                    cmd: Sys.Command,
                                    exitCode: Int,
  ) extends SysError

  final case class GenericError(
                                 cmd: Sys.Command,
                                 cause: Throwable,
  ) extends SysError

}
