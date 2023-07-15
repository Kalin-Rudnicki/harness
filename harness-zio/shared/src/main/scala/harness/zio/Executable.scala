package harness.zio

import cats.data.NonEmptyList
import cats.syntax.either.*
import harness.cli.*
import harness.core.*
import scala.annotation.tailrec
import zio.*

trait Executable { self =>

  def execute(args: IndexedArgs): SHTask[Any]

  final def apply(args: List[String]): UIO[ExitCode] = {
    val result: Executable.Result[(ULayer[HarnessEnv], SHTask[Any])] =
      for {
        args <- Executable.parseIndexedArgs(args)
        (harnessArgs, executableArgs) = Executable.parseSplitArgs(args)
        harnessEnvLayer <- Executable.parseLayer(harnessArgs)
        executableEffect = self.execute(executableArgs)
      } yield (harnessEnvLayer, executableEffect)

    result match {
      case Executable.Result.Success((layer, effect)) =>
        effect.collapseCause
          .tapError(e => Logger.log.debug(Logger.Event.Compound(e.toNel.toList.map(e => Logger.Event.Output(Map.empty, e.fullInternalMessageWithTrace)))))
          .dumpErrorsAndContinue(Logger.LogLevel.Fatal)
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
object Executable extends ExecutableBuilders.Builder1 {

  def fromSubCommands(opts: (String, Executable)*): Executable = {
    val map: Map[String, Executable] = opts.toMap

    {
      case Indexed(Arg.Value(subCommand), _) :: tail =>
        map.get(subCommand) match {
          case Some(executable) => executable.execute(tail)
          case None             => ZIO.fail(HError.UserError(s"Invalid sub-command '$subCommand', options: ${opts.map(_._1).mkString("[", ", ", "]")}"))
        }
      case _ =>
        ZIO.fail(HError.UserError(s"Missing sub-command, options: ${opts.map(_._1).mkString("[", ", ", "]")}"))
    }
  }

  // =====| Types |=====

  sealed trait Result[+T] {

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
  object Result {
    final case class Success[+T](value: T) extends Result[T]
    final case class Help(message: String) extends Result[Nothing]
    final case class Fail(error: HError) extends Result[Nothing]
  }

  private final case class Config(
      minLogTolerance: Logger.LogLevel,
      colorMode: ColorMode,
      runMode: RunMode,
      logJson: Boolean,
  )
  private object Config {

    val parser: Parser[Config] = {
      Parser
        .value[Logger.LogLevel](
          LongName.unsafe("min-log-tolerance"),
          Defaultable.Some(ShortName.unsafe('t')),
          helpHint = List("Disregard log messages below this level"),
        )
        .default(Logger.LogLevel.Info, Defaultable.Auto) &&
      Parser
        .value[ColorMode](
          LongName.unsafe("color-mode"),
          Defaultable.Some(ShortName.unsafe('c')),
          helpHint = List("Default logger colorization"),
        )
        .default(ColorMode.Extended, Defaultable.Auto) &&
      Parser
        .flag(
          LongName.unsafe("dev"),
          helpHint = List("Set RunMode to 'Dev'"),
        )
        .map {
          case true  => RunMode.Dev
          case false => RunMode.Prod
        } &&
      Parser
        .flag(
          LongName.unsafe("log-json"),
          shortParam = Defaultable.None,
          helpHint = List("Log messages in JSON format"),
        )
    }.map(Config.apply)

  }

  // =====| Helpers |=====

  private def parseIndexedArgs(args: List[String]): Result[IndexedArgs] =
    IndexedArgs.parse(args) match {
      case Left(msg)    => Result.Fail(HError.UserError(msg))
      case Right(value) => Result.Success(value)
    }

  def finalizedResultToExecutableResult[T](result: FinalizedParser.Result[T]): Executable.Result[T] =
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
        Logger.default(
          sources = (if (config.logJson) Logger.Source.stdOutJson(None) else Logger.Source.stdOut(None, None)) :: Nil,
          defaultMinLogTolerance = config.minLogTolerance,
          defaultColorMode = config.colorMode,
        )

      ZLayer.succeed(logger) ++
        ZLayer.succeed(Telemetry.log) ++
        ZLayer.succeed(config.runMode) ++
        ZLayer.succeed(HError.UserMessage.IfHidden.default) ++
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

object ExecutableBuilders {

  class Builder3[P, R: EnvironmentTag](
      parser: Parser[P],
      layer: P => SHRLayer[Scope, R],
  ) {

    final def withEffect(effect: P => SHRIO[R & Scope, Any]): Executable = { args =>
      Executable.finalizedResultToExecutableResult(parser.finalized.parse(args)) match {
        case Executable.Result.Success(parseT) =>
          ZIO.scoped {
            effect(parseT).provideSomeLayer(layer(parseT))
          }
        case Executable.Result.Help(message) => Logger.log.info(message)
        case Executable.Result.Fail(error)   => ZIO.fail(error)
      }
    }

    final def withEffect(effect: => SHRIO[R & Scope, Any]): Executable = this.withEffect { _ => effect }

  }

  class Builder2[P](
      parser: Parser[P],
  ) extends Builder3[P, Any](parser, _ => ZLayer.empty) {

    final def withLayer[R: EnvironmentTag](layer: P => SHRLayer[Scope, R]): Builder3[P, R] = new Builder3(parser, layer)

    final def withLayer[R: EnvironmentTag](layer: => SHRLayer[Scope, R]): Builder3[P, R] = this.withLayer { _ => layer }

  }

  class Builder1 extends Builder2[Unit](Parser.unit) {

    final def withParser[P](parser: Parser[P]): Builder2[P] = new Builder2(parser)

  }

  // TODO (KR) : Add functionality for specifying how [ these args -- a b c ] are parsed

}
