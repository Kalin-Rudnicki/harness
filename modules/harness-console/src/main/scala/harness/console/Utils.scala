package harness.console

import cats.data.NonEmptyList
import harness.console.Utils.ParseState.AwaitingArg
import harness.console.error.ExecuteError
import harness.core.*
import harness.zio.*
import scala.annotation.tailrec
import zio.*

object Utils {

  sealed trait ParseState
  object ParseState {
    sealed trait Returnable extends ParseState

    final case class AwaitingArg(rArgs: List[String]) extends ParseState.Returnable
    final case class Quoted(rChars: List[Char], rArgs: List[String]) extends ParseState.Returnable
    final case class NotQuoted(rChars: List[Char], rArgs: List[String]) extends ParseState
  }

  sealed trait GrabResult
  object GrabResult {
    final case class NowAndNext(now: Char, next: List[Char], escaped: Boolean) extends GrabResult
    case object EOL extends GrabResult
    case object EscapedEOL extends GrabResult
  }

  def grabResult(line: List[Char]): GrabResult =
    line match {
      case Nil                 => GrabResult.EOL
      case '\\' :: Nil         => GrabResult.EscapedEOL
      case '\\' :: 'n' :: rest => GrabResult.NowAndNext('\n', rest, true)
      case '\\' :: 't' :: rest => GrabResult.NowAndNext('\t', rest, true)
      case '\\' :: c :: rest   => GrabResult.NowAndNext(c, rest, true)
      case c :: rest           => GrabResult.NowAndNext(c, rest, false)
    }

  @tailrec
  def parseLine(state: ParseState, line: List[Char]): (ParseState.Returnable, Boolean) =
    (state, grabResult(line)) match {

      // (AwaitingArg, NowAndNext)
      case (ParseState.AwaitingArg(rArgs), GrabResult.NowAndNext(' ' | '\t', rest, false)) =>
        parseLine(ParseState.AwaitingArg(rArgs), rest)
      case (ParseState.AwaitingArg(rArgs), GrabResult.NowAndNext('`', rest, false)) =>
        parseLine(ParseState.Quoted(Nil, rArgs), rest)
      case (ParseState.AwaitingArg(rArgs), GrabResult.NowAndNext(c, rest, _)) =>
        parseLine(ParseState.NotQuoted(c :: Nil, rArgs), rest)

      // (Quoted, NowAndNext)
      case (ParseState.Quoted(rChars, rArgs), GrabResult.NowAndNext('`', rest, false)) =>
        parseLine(ParseState.NotQuoted(rChars, rArgs), rest)
      case (ParseState.Quoted(rChars, rArgs), GrabResult.NowAndNext(c, rest, _)) =>
        parseLine(ParseState.Quoted(c :: rChars, rArgs), rest)

      // (NotQuoted, NowAndNext)
      case (ParseState.NotQuoted(rChars, rArgs), GrabResult.NowAndNext(' ' | '\t', rest, false)) =>
        parseLine(ParseState.AwaitingArg(rChars.reverse.mkString :: rArgs), rest)
      case (ParseState.NotQuoted(rChars, rArgs), GrabResult.NowAndNext('`', rest, false)) =>
        parseLine(ParseState.Quoted(rChars, rArgs), rest)
      case (ParseState.NotQuoted(rChars, rArgs), GrabResult.NowAndNext(c, rest, _)) =>
        parseLine(ParseState.NotQuoted(c :: rChars, rArgs), rest)

      // (_, EOL)
      case (aa: ParseState.AwaitingArg, GrabResult.EOL)          => aa -> false
      case (ParseState.Quoted(rChars, rArgs), GrabResult.EOL)    => ParseState.Quoted('\n' :: rChars, rArgs) -> true
      case (ParseState.NotQuoted(rChars, rArgs), GrabResult.EOL) => ParseState.AwaitingArg(rChars.reverse.mkString :: rArgs) -> false

      // (_, EscapedEOL)
      case (ParseState.AwaitingArg(rArgs), GrabResult.EscapedEOL)       => ParseState.AwaitingArg(rArgs) -> true
      case (ParseState.Quoted(rChars, rArgs), GrabResult.EscapedEOL)    => ParseState.Quoted('\n' :: rChars, rArgs) -> true
      case (ParseState.NotQuoted(rChars, rArgs), GrabResult.EscapedEOL) => ParseState.AwaitingArg(rChars.reverse.mkString :: rArgs) -> true

    }

  def getInput(state: ParseState, first: Boolean): RIO[Logger, List[String]] =
    for {
      line <- Console.readLine(if (first) "Enter Command:\n> " else "| ")
      args <- parseLine(state, line.toList) match {
        case (AwaitingArg(rArgs), false) => ZIO.succeed(rArgs.reverse)
        case (state, _)                  => getInput(state, false)
      }
    } yield args

  @tailrec
  def executeProgram[R, C, S](
      programs: NonEmptyList[ConsoleProgram[R, C, S]],
      args: List[String],
      constState: C,
      stateRef: Ref[S],
  ): RIO[HarnessEnv & R, Unit] =
    args match {
      case command :: rest =>
        programs.find(_.command == command) match {
          case Some(program) =>
            program match {
              case ConsoleProgram.Cmd(cmd)          => cmd.execute(rest, constState, stateRef)
              case ConsoleProgram.OneOf(_, options) => executeProgram(options, rest, constState, stateRef)
            }
          case None => ZIO.fail(ExecuteError.InvalidCommand(command, programs.map(_.command)))
        }
      case Nil =>
        ZIO.fail(ExecuteError.MissingCommand(programs.map(_.command)))
    }

  def loop[R, C, S](program: ConsoleProgram[R, C, S], constState: C, stateRef: Ref[S]): URIO[HarnessEnv & R, Unit] =
    (for {
      args <- getInput(ParseState.AwaitingArg(Nil), true)
      _ <- Logger.log.debug(s"args (${args.size}):${args.map(a => s"\n  - ${a.unesc}").mkString}")
      exit <- args match {
        case "exit" :: Nil => ZIO.succeed(true)
        case _             => executeProgram(NonEmptyList.one(program), args, constState, stateRef).as(false)
      }
    } yield exit)
      .logErrorCauseSimpleAndContinue(Logger.LogLevel.Error, None)(using ErrorLogger.ThrowableInstances.getMessageErrorLogger)
      .flatMap {
        case Some(true) => ZIO.unit
        case _          => loop(program, constState, stateRef)
      }

}
