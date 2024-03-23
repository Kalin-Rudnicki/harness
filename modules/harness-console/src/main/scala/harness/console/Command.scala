package harness.console

import harness.cli.*
import harness.cli.FinalizedParser.Result
import harness.console.error.*
import harness.zio.*
import zio.*

trait Command[-R, C, S] {

  val command: String

  protected type P
  protected val parser: FinalizedParser[P]
  protected val effect: (P, C, S) => RIO[R & HarnessEnv, S]

  final def execute(args: List[String], constState: C, stateRef: Ref[S]): RIO[HarnessEnv & R, Unit] =
    (for {
      state <- stateRef.get
      parsed <- parser(args) match {
        case Result.Success(value) =>
          ZIO.succeed(value)
        case Result.ParamNameConflict(duplicateParam) =>
          ZIO.fail(CommandError.BuildingFail(s"Invalid parser: Duplicate param '${duplicateParam.showName}'"))
        case Result.Help(_, message) =>
          ZIO.fail(ShowHelp(message))
        case Result.ParseFail(fail) =>
          ZIO.fail(CommandError.ParsingFail(fail.toString))
        case Result.InvalidArg(msg) =>
          ZIO.fail(CommandError.ParsingFail(msg))
      }
      newState <- effect(parsed, constState, state)
      _ <- stateRef.set(newState)
    } yield ())
      .catchSome { case ShowHelp(help) => Logger.log.info(help.format()) }

}
object Command {

  def make[R, _P, C, S](
      _command: String,
      _parser: Parser[_P],
      _effect: (_P, C, S) => RIO[R & HarnessEnv, S],
  ): Command[R, C, S] =
    new Command[R, C, S] {
      override protected type P = _P
      override val command: String = _command
      override protected val parser: FinalizedParser[P] = _parser.finalized
      override protected val effect: (P, C, S) => RIO[R & HarnessEnv, S] = _effect
    }

}
