package harness.console

import cats.data.NonEmptyList
import harness.cli.Parser
import harness.zio.*
import zio.*

sealed trait ConsoleProgram[-R, C, S] {
  val command: String
}
object ConsoleProgram {

  final case class Cmd[-R, C, S](cmd: Command[R, C, S]) extends ConsoleProgram[R, C, S] {
    override val command: String = cmd.command
  }
  final case class OneOf[-R, C, S](command: String, options: NonEmptyList[ConsoleProgram[R, C, S]]) extends ConsoleProgram[R, C, S]

  // =====|  |=====

  def command(cmd: String): CommandBuilder1 = new CommandBuilder1(cmd)

  def oneOf[R, C, S](cmd: String)(p0: ConsoleProgram[R, C, S], pN: ConsoleProgram[R, C, S]*): ConsoleProgram[R, C, S] =
    ConsoleProgram.OneOf(cmd, NonEmptyList(p0, pN.toList))

  // =====|  |=====

  final class CommandBuilder1 private[ConsoleProgram] (cmd: String) {
    def withParser[P](parser: Parser[P]): CommandBuilder2[P] = new CommandBuilder2[P](cmd, parser)
    def noParser: CommandBuilder2[Unit] = withParser(Parser.unit)
  }

  final class CommandBuilder2[P] private[ConsoleProgram] (cmd: String, parser: Parser[P]) {
    def implement[C, S]: CommandBuilder3[P, C, S] = new CommandBuilder3[P, C, S](cmd, parser)
  }

  final class CommandBuilder3[P, C, S] private[ConsoleProgram] (cmd: String, parser: Parser[P]) {
    def apply[R](effect: (P, C, S) => RIO[HarnessEnv & R, S]): ConsoleProgram[R, C, S] =
      ConsoleProgram.Cmd(Command.make(cmd, parser, effect))
  }

}

extension [R, C, S](self: ConsoleProgram[R, C, S]) {

  def toExecutable(
      layer: RLayer[HarnessEnv, R],
      states: RIO[HarnessEnv & R, (C, S)],
  )(implicit rTag: EnvironmentTag[R]): Executable =
    Executable
      .withParser(Parser.unit) // TODO (KR) : potentially allow for calling the program with args such that it would execute the command and exit
      .withLayer { layer }
      .withThrowableEffect {
        for {
          (c, s) <- states
          statesRef <- Ref.make(s)
          _ <- Utils.loop(self, c, statesRef)
          _ <- Logger.log.info("exiting...")
        } yield ()
      }

}
