package harness.zio

import cats.data.NonEmptyList
import cats.syntax.either.*
import harness.cli.*
import harness.core.*
import scala.annotation.tailrec
import zio.*

trait Executable { self =>

  def execute(args: IndexedArgs): SHTaskN[Any]

  final def apply(args: List[String]): UIO[ExitCode] = {
    val result: Executable.Result[(ULayer[HarnessEnv], SHTaskN[Any])] =
      for {
        args <- Executable.parseIndexedArgs(args)
        (harnessArgs, executableArgs) = Executable.parseSplitArgs(args)
        harnessEnvLayer <- Executable.parseLayer(harnessArgs)
        executableEffect = self.execute(executableArgs)
      } yield (harnessEnvLayer, executableEffect)

    result match {
      case Executable.Result.Success((layer, effect)) =>
        effect
          .foldCauseZIO(
            {
              case fail @ Cause.Fail(_, _) => ZIO.failCause(fail)
              case Cause.Die(die, _)       => ZIO.failNel(HError.InternalDefect("ZIO died", die))
              case cause                   => ZIO.failNel(HError.InternalDefect(s"ZIO failed with cause:\n$cause"))
            },
            ZIO.succeed,
          )
          .dumpErrorsAndContinueNel(Logger.LogLevel.Fatal)
          .map {
            case Some(_) => ExitCode.success
            case None    => ExitCode.failure
          }
          .provideLayer(layer)
      case Executable.Result.Help(message) =>
        Logger.log.info(message).as(ExitCode.success).provideLayer(HarnessEnv.defaultLayer.orDie)
      case Executable.Result.Fail(error) =>
        Logger.logHError.fatal(error).as(ExitCode.failure).provideLayer(HarnessEnv.defaultLayer.orDie)
    }
  }

  final def apply(args: String*): UIO[ExitCode] =
    self(args.toList)

}
object Executable {

  // =====| Builders |=====

  def withParser[ParseT](parser: Parser[ParseT]): Builder1[ParseT] = Builder1(parser.finalized)

  final class Builder1[ParseT] private[Executable] (parser: FinalizedParser[ParseT]) {

    def withLayerNel[R: EnvironmentTag](layer: ParseT => ZLayer[HarnessEnv, NonEmptyList[HError], R]): Builder2[ParseT, R] = Builder2(parser, layer)
    inline def withLayerNel[R: EnvironmentTag](layer: => ZLayer[HarnessEnv, NonEmptyList[HError], R]): Builder2[ParseT, R] = this.withLayerNel { _ => layer }

    inline def withLayer[R: EnvironmentTag](layer: ParseT => ZLayer[HarnessEnv, HError, R]): Builder2[ParseT, R] = this.withLayerNel(layer(_).mapError(NonEmptyList.one))
    inline def withLayer[R: EnvironmentTag](layer: => ZLayer[HarnessEnv, HError, R]): Builder2[ParseT, R] = this.withLayer { _ => layer }

    def withEffectNel(effect: ParseT => ZIO[HarnessEnv, NonEmptyList[HError], Any]): Executable = { args =>
      Executable.finalizedResultToExecutableResult(parser.parse(args)) match {
        case Executable.Result.Success(parseT) => effect(parseT)
        case Executable.Result.Help(message)   => Logger.log.info(message)
        case Executable.Result.Fail(error)     => ZIO.failNel(error)
      }
    }
    inline def withEffectNel(effect: => ZIO[HarnessEnv, NonEmptyList[HError], Any]): Executable = this.withEffectNel { _ => effect }

    inline def withEffect(effect: ParseT => ZIO[HarnessEnv, HError, Any]): Executable = this.withEffectNel(effect(_).mapError(NonEmptyList.one))
    inline def withEffect(effect: => ZIO[HarnessEnv, HError, Any]): Executable = this.withEffect { _ => effect }

  }

  final class Builder2[ParseT, R: EnvironmentTag] private[Executable] (parser: FinalizedParser[ParseT], layer: ParseT => ZLayer[HarnessEnv, NonEmptyList[HError], R]) {

    def withEffectNel(effect: ParseT => ZIO[HarnessEnv & R, NonEmptyList[HError], Any]): Executable = { args =>
      Executable.finalizedResultToExecutableResult(parser.parse(args)) match {
        case Executable.Result.Success(parseT) => effect(parseT).provideSomeLayer(layer(parseT))
        case Executable.Result.Help(message)   => Logger.log.info(message)
        case Executable.Result.Fail(error)     => ZIO.failNel(error)
      }
    }
    inline def withEffectNel(effect: => ZIO[HarnessEnv & R, NonEmptyList[HError], Any]): Executable = this.withEffectNel { _ => effect }

    inline def withEffect(effect: ParseT => ZIO[HarnessEnv & R, HError, Any]): Executable = this.withEffectNel(effect(_).mapError(NonEmptyList.one))
    inline def withEffect(effect: => ZIO[HarnessEnv & R, HError, Any]): Executable = this.withEffect { _ => effect }

  }

  def fromSubCommands(opts: (String, Executable)*): Executable = {
    val map: Map[String, Executable] = opts.toMap

    {
      case Indexed(Arg.Value(subCommand), _) :: tail =>
        map.get(subCommand) match {
          case Some(executable) => executable.execute(tail)
          case None             => ZIO.failNel(HError.UserError(s"Invalid sub-command '$subCommand', options: ${opts.mkString("[", ", ", "]")}"))
        }
      case _ =>
        ZIO.failNel(HError.UserError(s"Missing sub-command, options: ${opts.mkString("[", ", ", "]")}"))
    }
  }

  // =====| Types |=====

  private sealed trait Result[+T] {

    final def map[T2](f: T => T2): Result[T2] =
      this match {
        case Result.Success(value) => Result.Success(f(value))
        case help: Result.Help     => help
        case fail: Result.Fail     => fail
      }

    final def flatMap[T2](f: T => Result[T2]): Result[T2] =
      this match {
        case Result.Success(value) => f(value)
        case help: Result.Help     => help
        case fail: Result.Fail     => fail
      }

  }
  private object Result {
    final case class Success[+T](value: T) extends Result[T]
    final case class Help(message: String) extends Result[Nothing]
    final case class Fail(error: HError) extends Result[Nothing]
  }

  private final case class Config(
      minLogTolerance: Logger.LogLevel,
      maxLogTolerance: Logger.LogLevel,
      runMode: RunMode,
  )
  private object Config {

    val parser: Parser[Config] = {
      Parser
        .value[Logger.LogLevel](
          LongName.unsafe("min-log-tolerance"),
          Defaultable.Some(ShortName.unsafe('t')),
          helpHint = List("Disregard log messages below this level"),
        )
        .default(Logger.LogLevel.Info, true) &&
      Parser
        .value[Logger.LogLevel](
          LongName.unsafe("max-log-tolerance"),
          Defaultable.Some(ShortName.unsafe('T')),
          helpHint = List("Disregard log messages above this level"),
        )
        .default(Logger.LogLevel.Always, true) &&
      Parser
        .flag(
          LongName.unsafe("dev"),
          helpHint = List("Set RunMode to 'Dev'"),
        )
        .map {
          case true  => RunMode.Dev
          case false => RunMode.Prod
        }
    }.map(Config.apply)

  }

  // =====| Helpers |=====

  private def parseIndexedArgs(args: List[String]): Result[IndexedArgs] =
    IndexedArgs.parse(args) match {
      case Left(msg)    => Result.Fail(HError.UserError(msg))
      case Right(value) => Result.Success(value)
    }

  private def finalizedResultToExecutableResult[T](result: FinalizedParser.Result[T]): Executable.Result[T] =
    result match {
      case FinalizedParser.Result.Success(value)            => Result.Success(value)
      case FinalizedParser.Result.Help(_, message)          => Result.Help(message.format())
      case FinalizedParser.Result.ParseFail(fail)           => Result.Fail(HError.UserError(fail.toString))
      case FinalizedParser.Result.BuildFail(duplicateParam) => Result.Fail(HError.InternalDefect(s"Parser declares duplicate param: $duplicateParam"))
      case FinalizedParser.Result.InvalidArg(msg)           => Result.Fail(HError.UserError(msg))
    }

  private def parseLayer(args: IndexedArgs): Result[ULayer[HarnessEnv]] =
    finalizedResultToExecutableResult(Config.parser.finalized.parse(args)).map { config =>
      val logger: Logger =
        Logger(
          sources = Logger.Source.stdOut(config.minLogTolerance, config.maxLogTolerance) :: Nil,
        )

      ZLayer.succeed(logger) ++
        ZLayer.succeed(config.runMode) ++
        FileSystem.liveLayer.orDie
    }

  private def parseSplitArgs(args: IndexedArgs): (IndexedArgs, IndexedArgs) = {
    @tailrec
    def loop(
        queue: IndexedArgs,
        stack: IndexedArgs,
    ): (IndexedArgs, IndexedArgs) =
      queue match {
        case Indexed(Arg.Value("--"), _) :: tail => (stack.reverse, tail)
        case head :: tail                        => loop(tail, head :: stack)
        case Nil                                 => (Nil, stack.reverse)
      }

    loop(args, Nil)
  }

}
