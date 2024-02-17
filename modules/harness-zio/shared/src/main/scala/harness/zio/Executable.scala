package harness.zio

import cats.syntax.either.*
import cats.syntax.option.*
import harness.cli.*
import harness.cli.FinalizedParser.Result
import harness.core.*
import harness.zio.error.ExecutableError
import scala.annotation.tailrec
import scala.util.matching.Regex
import zio.*
import zio.json.*
import zio.json.ast.Json

trait Executable private { self =>

  protected def executeInternal(args: IndexedArgs): ZIO[HarnessEnv & Scope, ExecutableError.Internal, ExitCode]

  final def apply(config: ExecutableApp.Config, args: List[String]): UIO[ExitCode] = {
    val result: Executable.Result[(ZLayer[Scope, ExecutableError.Internal, HarnessEnv], IndexedArgs)] =
      for {
        args <- Executable.parseIndexedArgs(args)
        (harnessArgs, executableArgs) = Executable.parseSplitArgs(args)
        harnessEnvLayer <- Executable.parseHarnessEnvLayer(config, harnessArgs)
      } yield (harnessEnvLayer, executableArgs)

    val execute: ZIO[Logger & Scope, ExecutableError.Internal, ExitCode] =
      result match {
        case Executable.Result.Success((layer, effectArgs)) =>
          executeInternal(effectArgs)
            .logErrorCauseSimpleAndContinue(Logger.LogLevel.Fatal, Logger.LogLevel.Debug.some)(ExecutableError.errorLogger(ErrorLogger.nothingErrorLogger))
            .map {
              case Some(exitCode) => exitCode
              case None           => ExitCode.failure
            }
            .provideSomeLayer[Scope](layer)
        case Executable.Result.Help(message) =>
          Logger.log.info(message).as(ExitCode.success)
        case Executable.Result.Fail(error) =>
          Logger.logError(error).as(ExitCode.failure)
      }

    execute
      .catchAllCause { error =>
        Logger.logErrorCauseSimple(error, Logger.LogLevel.Fatal, Logger.LogLevel.Debug.some).as(ExitCode.failure)
      }
      .provideLayer(Scope.default ++ ZLayer.succeed(Logger.default()))
  }

  final def apply(config: ExecutableApp.Config)(args: String*): UIO[ExitCode] =
    self(config, args.toList)

}
object Executable extends ExecutableBuilders.Builder1 {

  def fromSubCommands(opts: (String, Executable)*): Executable = {
    val map: Map[String, Executable] = opts.toMap

    new Executable {
      override protected def executeInternal(args: IndexedArgs): ZIO[HarnessEnv & Scope, ExecutableError.Internal, ExitCode] =
        args match {
          // TODO (KR) : Do something nicer here
          case Indexed(arg, _) :: _ if arg == Arg.ShortParamSingle(ShortName.unsafe('H')) || arg == Arg.LongParam(LongName.unsafe("help-extra")) =>
            Logger.log.debug("TODO : Nicer handling of sub-commands") *>
              ZIO.fail(ExecutableError.MissingSubCommand(opts.map(_._1).toList))
          case Indexed(arg, _) :: _ if arg == Arg.ShortParamSingle(ShortName.unsafe('h')) || arg == Arg.LongParam(LongName.unsafe("help")) =>
            Logger.log.debug("TODO : Nicer handling of sub-commands") *>
              ZIO.fail(ExecutableError.MissingSubCommand(opts.map(_._1).toList))

          case Indexed(Arg.Value(subCommand), _) :: tail =>
            map.get(subCommand) match {
              case Some(executable) => executable.executeInternal(tail)
              case None             => ZIO.fail(ExecutableError.InvalidSubCommand(subCommand, opts.map(_._1).toList))
            }
          case _ =>
            ZIO.fail(ExecutableError.MissingSubCommand(opts.map(_._1).toList))
        }
    }
  }

  def fromExecute[E: ErrorLogger](f: IndexedArgs => ZIO[HarnessEnv & Scope, ExecutableError[E], Any]): Executable =
    new Executable {
      override protected def executeInternal(args: IndexedArgs): ZIO[HarnessEnv & Scope, ExecutableError.Internal, ExitCode] =
        f(args)
          .logErrorCauseSimpleAndContinue(Logger.LogLevel.Fatal, Logger.LogLevel.Debug.some)(ExecutableError.errorLogger(ErrorLogger[E]))
          .map {
            case Some(_) => ExitCode.success
            case None    => ExitCode.failure
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
    final case class Fail(error: ExecutableError.Internal) extends Result[Nothing]
  }

  private final case class Config(
      configStrings: List[Config.ConfigString],
      stdOutLogTolerance: Option[Logger.LogLevel],
  )
  private object Config {

    sealed trait ConfigString
    object ConfigString {
      final case class Env(varName: String) extends ConfigString
      final case class FilePath(path: String) extends ConfigString
      final case class JarResPath(path: String) extends ConfigString
      final case class JsonConfig(json: Json) extends ConfigString

      private def parseJson(json: String): Json =
        json.fromJson[zio.json.ast.Json].toOption.getOrElse(Json.Str(json))

      private def nestJson(nest: String, json: Json): Json = {
        @tailrec
        def loop(
            nest: List[String],
            json: Json,
        ): Json =
          nest match {
            case head :: tail => loop(tail, Json.Obj(head -> json))
            case Nil          => json
          }

        loop(nest.split('.').toList.reverse, json)
      }

      private val envResRegex: Regex = "^env:(.*)$".r
      private val jarResRegex: Regex = "^jar:(.*)$".r
      private val fileResRegex: Regex = "^file:(.*)$".r
      private val jsonPathRegex: Regex = "^json:([A-Za-z_\\-0-9]+(?:\\.[A-Za-z_\\-0-9]+)*):(.*)$".r
      private val jsonRegex: Regex = "^json:(.*)$".r
      implicit val stringDecoder: StringDecoder[ConfigString] =
        StringDecoder.string.flatMap {
          case envResRegex(varName)      => ConfigString.Env(varName).asRight
          case jarResRegex(path)         => ConfigString.JarResPath(path).asRight
          case fileResRegex(path)        => ConfigString.FilePath(path).asRight
          case jsonPathRegex(nest, json) => ConfigString.JsonConfig(nestJson(nest, parseJson(json))).asRight
          case jsonRegex(json)           => ConfigString.JsonConfig(parseJson(json)).asRight
          case _                         => "does not match a valid regex".leftNel
        }

    }

    val parser: Parser[Config] = {
      Parser.values.list[Config.ConfigString](
        LongName.unsafe("config-path"),
        Defaultable.Some(ShortName.unsafe('C')),
        helpHint = List(
          "jar:path/to/jar/res",
          "file:path/to/file",
          """json:{ "js": { "path": "some-json" } }""",
          """json:js.path:"some-json"""",
        ),
      ) &&
      Parser
        .value[Logger.LogLevel](
          LongName.unsafe("std-out-log-tolerance"),
          Defaultable.Some(ShortName.unsafe('t')),
          helpHint = List("Don't look for logger/telemetry in config, and instead just use std-out"),
        )
        .optional
    }.map(Config.apply)

  }

  // =====| Helpers |=====

  private def parseIndexedArgs(args: List[String]): Result[IndexedArgs] =
    IndexedArgs.parse(args) match {
      case Right(value) => Result.Success(value)
      case Left(msg)    => Result.Fail(ExecutableError.FailedToParseIndexedArgs(msg))
    }

  def finalizedResultToExecutableResult[T](result: FinalizedParser.Result[T]): Executable.Result[T] =
    result match {
      case FinalizedParser.Result.Success(value)         => Result.Success(value)
      case nonSuccess: FinalizedParser.Result.NonSuccess => Executable.Result.Fail(ExecutableError.ParsingFailure(nonSuccess))
    }

  private def loadConfig(cfg: Config.ConfigString): ZIO[FileSystem, ExecutableError.FailedToLoadConfig, HConfig] =
    (cfg match {
      case Config.ConfigString.Env(varName)     => HConfig.fromEnvVar(varName)
      case Config.ConfigString.FilePath(path)   => HConfig.fromPathString(path)
      case Config.ConfigString.JarResPath(path) => HConfig.fromJarResource(path)
      case Config.ConfigString.JsonConfig(json) => ZIO.succeed(HConfig.fromJson(json))
    }).mapError(ExecutableError.FailedToLoadConfig(_))

  private def loadConfigs(configStrings: List[Config.ConfigString]): ZLayer[FileSystem, ExecutableError.FailedToLoadConfig, HConfig] =
    ZLayer.fromZIO { ZIO.foreach(configStrings)(loadConfig).map(HConfig.flatten) }

  private def parseHarnessEnvLayer(
      executableAppConfig: ExecutableApp.Config,
      args: IndexedArgs,
  ): Result[ZLayer[Scope, ExecutableError.Internal, HarnessEnv]] =
    finalizedResultToExecutableResult(Config.parser.finalized.parse(args)).map { config =>
      implicit val loggerConfigJsonDecoder: JsonDecoder[LoggerConfig] = LoggerConfig.jsonDecoder(executableAppConfig.loggerDecoders*)
      implicit val telemetryConfigJsonDecoder: JsonDecoder[TelemetryConfig] = TelemetryConfig.jsonDecoder(executableAppConfig.telemetryDecoders*)

      val loggerAndTelemetryLayer: ZLayer[HConfig & Scope, ExecutableError.FailedToReadConfig, Logger & Telemetry] =
        config.stdOutLogTolerance match {
          case Some(stdOutLogTolerance) =>
            ZLayer.make[Logger & Telemetry](
              ZLayer.succeed(Logger.default(defaultMinLogTolerance = stdOutLogTolerance)),
              ZLayer.succeed(Telemetry.log),
            )
          case None =>
            ZLayer.makeSome[HConfig & Scope, Logger & Telemetry](
              HConfig.readLayer[LoggerConfig]("logging").mapError(ExecutableError.FailedToReadConfig(_)),
              Logger.configLayer,
              HConfig.readLayer[TelemetryConfig]("telemetry").mapError(ExecutableError.FailedToReadConfig(_)),
              Telemetry.configLayer,
            )
        }

      ZLayer.makeSome[Scope, HarnessEnv](
        FileSystem.liveLayer.orDie,
        loadConfigs(config.configStrings),
        loggerAndTelemetryLayer,
      )
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

  class Builder3[P, R: EnvironmentTag, E](
      parser: Parser[P],
      layer: P => ZLayer[HarnessEnv & Scope, E, R],
  ) {

    final def withEffect[E2 >: E: ErrorLogger](effect: P => ZIO[HarnessEnv & R & Scope, E2, Any]): Executable =
      Executable.fromExecute { args =>
        Executable.finalizedResultToExecutableResult(parser.finalized.parse(args)) match {
          case Executable.Result.Success(parseT) => effect(parseT).provideSomeLayer(layer(parseT)).mapError(ExecutableError.External(_))
          case Executable.Result.Help(message)   => Logger.log.info(message)
          case Executable.Result.Fail(error)     => ZIO.fail(error)
        }
      }

    final def withEffect[E2 >: E: ErrorLogger](effect: => ZIO[HarnessEnv & R & Scope, E2, Any]): Executable = this.withEffect[E2] { _ => effect }

    final def withEffectSimple[E2 >: E <: Throwable](effect: P => ZIO[HarnessEnv & R & Scope, E2, Any]): Executable =
      this.withEffect[E2] { effect }(using ErrorLogger.throwablePrettyErrorLogger)

    final def withEffectSimple[E2 >: E <: Throwable](effect: => ZIO[HarnessEnv & R & Scope, E2, Any]): Executable =
      this.withEffect[E2] { _ => effect }(using ErrorLogger.throwablePrettyErrorLogger)

  }

  class Builder2[P](
      parser: Parser[P],
  ) extends Builder3[P, Any, Nothing](parser, _ => ZLayer.empty) {

    final def withLayer[R: EnvironmentTag, E](layer: P => ZLayer[HarnessEnv & Scope, E, R]): Builder3[P, R, E] = new Builder3(parser, layer)

    final def withLayer[R: EnvironmentTag, E](layer: => ZLayer[HarnessEnv & Scope, E, R]): Builder3[P, R, E] = this.withLayer[R, E] { _ => layer }

  }

  class Builder1 extends Builder2[Unit](Parser.unit) {

    final def withParser[P](parser: Parser[P]): Builder2[P] = new Builder2(parser)

  }

  // TODO (KR) : Add functionality for specifying how [ these args -- a b c ] are parsed

}
