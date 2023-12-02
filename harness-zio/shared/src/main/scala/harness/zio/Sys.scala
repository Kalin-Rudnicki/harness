package harness.zio

import cats.data.NonEmptyList
import harness.core.*
import scala.sys.process.*
import zio.*

object Sys {

  final case class Command(cmdAndArgs: NonEmptyList[String]) {
    def sudo: Command = Command("sudo" :: cmdAndArgs)
    def sudoIf(cond: Boolean): Command = if (cond) this.sudo else this
    def :+(tail: List[String]): Command = Command(cmdAndArgs ++ tail)
  }
  object Command {
    def apply(cmd: String, args: String*): Command =
      new Command(NonEmptyList(cmd, args.toList))
    def apply(cmd: String, arg0: Option[List[String]], argN: Option[List[String]]*): Command =
      new Command(NonEmptyList(cmd, (arg0 :: argN.toList).flatMap(_.toList.flatten)))
  }

  abstract class CmdBuilder[O](run: NonEmptyList[String] => HTask[O]) {
    def apply(cmdAndArgs: NonEmptyList[String]): HTask[O] = run(cmdAndArgs)
    def apply(cmd: Command): HTask[O] = run(cmd.cmdAndArgs)
    def apply(cmd: String, args: String*): HTask[O] = run(NonEmptyList(cmd, args.toList))
  }

  object execute
      extends CmdBuilder[Int](cmdAndArgs => ZIO.hAttempt { cmdAndArgs.toList.! }.mapError(HError.SystemFailure(s"Unable to run command: ${cmdAndArgs.toList.map(_.unesc).mkString(" ")}", _)))

  object execute0
      extends CmdBuilder[Unit](cmdAndArgs =>
        Sys
          .execute(cmdAndArgs)
          .flatMap {
            case 0    => ZIO.unit
            case code => ZIO.fail(HError.SystemFailure(s"Cmd '${cmdAndArgs.head}' executed with non-0 code '$code'"))
          },
      )

  object executeString0
      extends CmdBuilder[String](cmdAndArgs => ZIO.hAttempt { cmdAndArgs.toList.!! }.mapError(HError.SystemFailure(s"Unable to run command ${cmdAndArgs.toList.map(_.unesc).mkString(" ")}", _)))

}
