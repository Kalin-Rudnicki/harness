package harness.zio

import cats.data.NonEmptyList
import cats.syntax.either.*
import cats.syntax.option.*
import harness.cli.*
import harness.cli.Parser.FinalParseResult
import harness.core.*
import harness.zio.config.{EncodedConfigSource, LoggerConfig, TelemetryConfig}
import harness.zio.error.ExecutableError
import scala.annotation.tailrec
import zio.*
import zio.json.*
import zio.json.ast.Json
import zio.json.internal.RetractReader

sealed trait Executable {

  final def apply(args: List[String]): UIO[ExitCode] =
    ZIO.scoped {
      runRoot(args).stacklessFailures.logErrorDiscard
        .cause(Logger.LogLevel.Fatal)
        .someOrElse(ExitCode.failure)
    }
  final def apply(args: String*): UIO[ExitCode] =
    apply(args.toList)

  private final def runRoot(args: List[String]): ZIO[Scope, ExecutableError, ExitCode] =
    for {
      ((rawHarnessValues, rawHarnessParams), (rawExecutableValues, rawExecutableParams)) <- ZIO.fromEither(Executable.parseSplitArgs(args))
      config <- Executable.loadConfig(rawHarnessValues, rawHarnessParams)
      exitCode <- execute(config, rawExecutableValues, rawExecutableParams)
    } yield exitCode

  private[Executable] def execute(config: Json, values: List[Arg.ValueLike], params: List[Arg.ParamLike]): ZIO[Scope, ExecutableError, ExitCode]

}
object Executable extends ExecutableBuilders.Builder1 {

  trait Single extends Executable {

    protected type JsonConfig
    protected type CommandLineConfig

    protected type Env
    protected type Error

    protected implicit val jsonConfigDecoder: JsonDecoder[JsonConfig]
    protected implicit val errorLogger: ErrorLogger[Error]
    protected implicit val envTag: EnvironmentTag[Env]
    protected val commandLineParser: Parser[CommandLineConfig]

    // TODO (KR) :
    // protected val description: String

    protected def logger(c1: JsonConfig, c2: CommandLineConfig): RIO[Scope, Logger]
    protected def telemetry(c1: JsonConfig, c2: CommandLineConfig): RIO[Scope, Telemetry] = ZIO.succeed(Telemetry.log)
    protected def fileSystem(c1: JsonConfig, c2: CommandLineConfig): RIO[Scope, FileSystem] = ZIO.succeed(FileSystem.defaultFS)

    protected def myEnv(c1: JsonConfig, c2: CommandLineConfig): ZLayer[Scope, Error, Env]

    protected def run(c1: JsonConfig, c2: CommandLineConfig): ZIO[Env & Scope, Error, Unit]

    override private[Executable] final def execute(config: Json, values: List[Arg.ValueLike], params: List[Arg.ParamLike]): ZIO[Scope, ExecutableError, ExitCode] =
      for {
        jsonConfig <- ZIO.fromEither(config.toString.fromJson[JsonConfig](using jsonConfigDecoder)).mapError(ExecutableError.FailedToReadConfig(_))
        commandLineConfig <- Executable.parseArgs(commandLineParser, values, params)
        _ <- logger(jsonConfig, commandLineConfig).mapError(ExecutableError.FailedToCreateService("logger", _)).flatMap(Logger.loggerRef.locallyScoped)
        _ <- telemetry(jsonConfig, commandLineConfig).mapError(ExecutableError.FailedToCreateService("telemetry", _)).flatMap(Telemetry.telemetryRef.locallyScoped)
        _ <- fileSystem(jsonConfig, commandLineConfig).mapError(ExecutableError.FailedToCreateService("file-system", _)).flatMap(FileSystem.fileSystemRef.locallyScoped)

        result <-
          run(jsonConfig, commandLineConfig)
            .provideSomeLayer[Scope](myEnv(jsonConfig, commandLineConfig))
            .logErrorDiscard
            .simpleCause(Logger.LogLevel.Fatal)
            .map { case Some(_) => ExitCode.success; case None => ExitCode.failure }
      } yield result

  }

  final case class OneOf(options: Chunk[(String, Executable)]) extends Executable {

    private val optionMap: Map[String, Executable] = options.toMap

    // TODO (KR) : prettier errors in the case of `--help`
    override private[Executable] def execute(config: Json, values: List[Arg.ValueLike], params: List[Arg.ParamLike]): ZIO[Scope, ExecutableError, ExitCode] =
      values match
        case Arg.Value(_, optionMap(subExe)) :: valuesT => subExe.execute(config, valuesT, params)
        case Arg.Value(_, command) :: _                 => ZIO.fail(ExecutableError.InvalidSubCommand(command, options.map(_._1).toList))
        case _                                          => ZIO.fail(ExecutableError.MissingSubCommand(options.map(_._1).toList))

  }

  def fromSubCommands(opts: (String, Executable)*): Executable = OneOf(Chunk.fromIterable(opts))

  // =====| Config |=====

  final case class CommandLineLoggerConfig(
      logLevel: Logger.LogLevel,
      logTypes: NonEmptyList[CommandLineLoggerConfig.LogType],
      logContext: Logger.LogContext,
  ) {

    def toLogger: Logger = Logger(
      sources = Chunk.fromIterable(logTypes.toList).flatMap(_.source),
      context = Chunk.empty,
      defaultMinLogTolerance = logLevel,
      forwardToZio = logTypes.exists(_.forwardToZio),
    )

  }
  object CommandLineLoggerConfig {

    sealed trait LogType {

      private[CommandLineLoggerConfig] final def source: Option[Logger.Source] = this match
        case LogType.StdOut(colorMode, logTimestamp, logTrace, logStack) => Logger.Source.stdOut(None, colorMode, logTimestamp, logTrace, logStack).some
        case LogType.StdOutJson                                          => Logger.Source.stdOutJson(None).some
        case LogType.Zio                                                 => None

      private[CommandLineLoggerConfig] final def forwardToZio: Boolean = this match
        case LogType.Zio => true
        case _           => false

    }
    object LogType {

      final case class StdOut(
          colorMode: ColorMode,
          logTimestamp: Boolean,
          logTrace: Boolean,
          logStack: Boolean,
      ) extends LogType
      object StdOut {

        val parser: Params[StdOut] =
          (
            Params.ifPresent(
              "log-std-out",
              (),
              Defaultable.None,
              hints = List("Log to std-out in nice formatted fashion"),
            ) &&
              Params
                .value[ColorMode](
                  "log-color-mode",
                  Defaultable.None,
                  hints = List("Color Mode for logging to std-out"),
                )
                .withDefault(ColorMode.Extended) &&
              Params.toggle
                .prefixFalse(
                  "no",
                  "log-timestamp",
                  Defaultable.None,
                  hints = List("Log timestamp in std-out"),
                )
                .withDefault(false) &&
              Params.toggle
                .prefixFalse(
                  "no",
                  "log-trace",
                  Defaultable.None,
                  hints = List("Log trace location in std-out"),
                )
                .withDefault(false) &&
              Params.toggle
                .prefixFalse(
                  "no",
                  "log-stack",
                  Defaultable.None,
                  hints = List("Log stack-trace in std-out (if present)"),
                )
                .withDefault(true)
          ).map { StdOut.apply }

      }

      case object StdOutJson extends LogType {

        val parser: Params[StdOutJson.type] =
          Params.ifPresent(
            "log-std-out-json",
            StdOutJson,
            Defaultable.None,
            hints = List("Log to std-out in json format"),
          )

      }

      case object Zio extends LogType {

        val parser: Params[Zio.type] =
          Params.ifPresent(
            "log-zio",
            Zio,
            Defaultable.None,
            hints = List("Forward logs to the default zio logger"),
          )

      }

      implicit val parser: Params[LogType] =
        Params.firstOf[LogType](
          LogType.StdOut.parser,
          LogType.StdOutJson.parser,
          LogType.Zio.parser,
        )

    }

    private val logContextTupleRegex = "^([^:]+):([^:]+)$".r
    private val logContextTupleDecoder: StringDecoder[(String, String)] =
      StringDecoder.fromOptionF(
        "LogContextTuple",
        {
          case logContextTupleRegex(key, value) => (key, value).some
          case _                                => None
        },
      )

    def parser(
        logLevelDefault: Option[Logger.LogLevel] = Logger.LogLevel.Info.some,
        logTypeDefault: Option[LogType] = LogType.StdOut(ColorMode.Extended, false, false, true).some,
    ): Params[CommandLineLoggerConfig] =
      (
        Params
          .`enum`[Logger.LogLevel, String]("log-level", 'L', hints = List("Minimum log-level allowed for log messages"))
          .withOptionalDefault(logLevelDefault) &&
          LogType.parser.repeatedNel.withOptionalDefault(logTypeDefault.map(NonEmptyList.one)) &&
          Params
            .valueWith("log-context", 'C', hints = List("Default logger context"))(
              Values.value[(String, String)]("key-value", hints = "format:  key:value" :: Nil)(using logContextTupleDecoder) <||
                (Values.value[String]("key") ^>> Values.value[String]("value")),
            )
            .repeated
            .map(Chunk.fromIterable)
      ).map(CommandLineLoggerConfig.apply)

  }

  // =====| Helpers |=====

  private def parseSplitArgs(args: List[String]): Either[ExecutableError.FailedToParseArgs, ((List[Arg.ValueLike], List[Arg.ParamLike]), (List[Arg.ValueLike], List[Arg.ParamLike]))] = {
    @tailrec
    def loop(
        queue: List[String],
        stack: List[String],
    ): (List[String], List[String]) = queue match
      case "--" :: tail => (stack.reverse, tail)
      case head :: tail => loop(tail, head :: stack)
      case Nil          => (Nil, stack.reverse)

    val (harnessArgs, executableArgs) = loop(args, Nil)
    (for {
      harnessArgs <- Arg.parse(harnessArgs)
      executableArgs <- Arg.parse(executableArgs)
    } yield (harnessArgs, executableArgs)).leftMap(ExecutableError.FailedToParseArgs(_))
  }

  private def loadConfig(harnessValues: List[Arg.ValueLike], harnessParams: List[Arg.ParamLike]): IO[ExecutableError, Json] =
    for {
      config <- parseArgs(ExecutableConfig.parser, harnessValues, harnessParams)
      sources <- ZIO.foreach(config.configSources)(_.read.mapError(ExecutableError.FailedToLoadConfig(_)))
    } yield sources.foldLeft(Json.Obj(): Json)(_.merge(_))

  private def parseArgs[A](parser: Parser[A], values: List[Arg.ValueLike], params: List[Arg.ParamLike]): IO[ExecutableError, A] =
    parser.build match {
      case Right(parser) =>
        parser.parse(values, params).toFinal match {
          case FinalParseResult.Success(Right(value), _) => ZIO.succeed(value)
          case FinalParseResult.Success(Left(_), _)      => ZIO.fail(ExecutableError.CommandLineHelp(parser.helpMessage)) // TODO (KR) : help/help-extra
          case FinalParseResult.Fail(_, help)            => ZIO.fail(ExecutableError.CommandLineParsingFailure(help, parser.helpMessage))
        }
      case Left(buildError) => ZIO.fail(ExecutableError.InvalidParser(buildError))
    }

  private final case class ExecutableConfig(
      configSources: List[EncodedConfigSource[Json]],
  )
  private object ExecutableConfig {

    val parser: Parser[ExecutableConfig] =
      (
        EncodedConfigSource.parser[Json](using StringDecoder.fromJsonDecoder[Json]).repeated
      ).map { ExecutableConfig.apply }

  }

}

// =====| Builders |=====

object ExecutableBuilders {

  import Executable.CommandLineLoggerConfig

  private val unitJsonDecoder: JsonDecoder[Unit] =
    new JsonDecoder[Unit] {
      override def unsafeDecode(trace: List[JsonError], in: RetractReader): Unit = ()
    }

  // Needs Error
  class Builder1 extends Builder2[Throwable](ErrorLogger.throwableGetMessage[Throwable].atLevel.fatal) {

    final def withThrowableError: Builder2[Throwable] = this

    final def withError[Error](implicit e: ErrorLogger[Error]): Builder2[Error] =
      new Builder2[Error](e)

    final def withoutError: Builder2[Nothing] =
      withError[Nothing]

  }

  // Needs JsonConfig
  class Builder2[Error](
      errorLogger: ErrorLogger[Error],
  ) extends Builder3[Error, Unit](
        errorLogger,
        unitJsonDecoder,
      ) {

    final def withoutConfig: Builder3[Error, Unit] = this

    final def withConfig[JsonConfig](implicit jsonConfig: JsonDecoder[JsonConfig]): Builder3[Error, JsonConfig] =
      new Builder3(errorLogger, jsonConfig)

  }

  // Needs CommandLineConfig
  class Builder3[Error, JsonConfig](
      errorLogger: ErrorLogger[Error],
      jsonConfig: JsonDecoder[JsonConfig],
  ) extends Builder4[Error, JsonConfig, Unit](
        errorLogger,
        jsonConfig,
        Parser.unit,
      ) {

    final def withoutCommandLine: Builder4[Error, JsonConfig, Unit] = this

    final def withCommandLine[CommandLineConfig](parser: Parser[CommandLineConfig]): Builder4[Error, JsonConfig, CommandLineConfig] =
      new Builder4(errorLogger, jsonConfig, parser)

  }

  // Needs Logger
  class Builder4[Error, JsonConfig, CommandLineConfig](
      errorLogger: ErrorLogger[Error],
      jsonConfig: JsonDecoder[JsonConfig],
      parser: Parser[CommandLineConfig],
  ) extends Builder5[Error, JsonConfig, (CommandLineLoggerConfig, CommandLineConfig), CommandLineConfig](
        errorLogger,
        jsonConfig,
        CommandLineLoggerConfig.parser() ^>> parser,
        _._2,
        (_, clc) => ZIO.succeed(clc._1.toLogger),
      ) {

    final def withCommandLineLogger: Builder5[Error, JsonConfig, (CommandLineLoggerConfig, CommandLineConfig), CommandLineConfig] = this

    // should take the same params as CommandLineLoggerConfig.parser
    final def withConstLogger(
        logLevelDefault: Logger.LogLevel = Logger.LogLevel.Info,
        logTypeDefault: CommandLineLoggerConfig.LogType = CommandLineLoggerConfig.LogType.StdOut(ColorMode.Extended, true, true, true),
    )(
        logContext: (String, Any)*,
    ): Builder5[Error, JsonConfig, CommandLineConfig, CommandLineConfig] =
      new Builder5(
        errorLogger,
        jsonConfig,
        parser,
        identity,
        (_, _) => ZIO.succeed(CommandLineLoggerConfig(logLevelDefault, NonEmptyList.one(logTypeDefault), Logger.LogContext(logContext)).toLogger),
      )

    final def withConstLogger(logger: Logger): Builder5[Error, JsonConfig, CommandLineConfig, CommandLineConfig] =
      new Builder5(
        errorLogger,
        jsonConfig,
        parser,
        identity,
        (_, _) => ZIO.succeed(logger),
      )

    final def withConfigLogger(f: JsonConfig => LoggerConfig): Builder5[Error, JsonConfig, CommandLineConfig, CommandLineConfig] =
      new Builder5(errorLogger, jsonConfig, parser, identity, (jc, _) => f(jc).logger)

  }

  // Needs Telemetry
  class Builder5[Error, JsonConfig, RawCommandLineConfig, CommandLineConfig](
      errorLogger: ErrorLogger[Error],
      jsonConfig: JsonDecoder[JsonConfig],
      parser: Parser[RawCommandLineConfig],
      commandLineFromRaw: RawCommandLineConfig => CommandLineConfig,
      logger: (JsonConfig, RawCommandLineConfig) => RIO[Scope, Logger],
  ) extends Builder6[Error, JsonConfig, RawCommandLineConfig, CommandLineConfig](
        errorLogger,
        jsonConfig,
        parser,
        commandLineFromRaw,
        logger,
        (_, _) => ZIO.succeed(Telemetry.log),
      ) {

    final def withDefaultTelemetry: Builder6[Error, JsonConfig, RawCommandLineConfig, CommandLineConfig] = this

    final def withConfigTelemetry(f: JsonConfig => TelemetryConfig): Builder6[Error, JsonConfig, RawCommandLineConfig, CommandLineConfig] =
      new Builder6(errorLogger, jsonConfig, parser, commandLineFromRaw, logger, (jc, _) => f(jc).telemetry)

  }

  // Needs Env
  class Builder6[Error, JsonConfig, RawCommandLineConfig, CommandLineConfig](
      errorLogger: ErrorLogger[Error],
      jsonConfig: JsonDecoder[JsonConfig],
      parser: Parser[RawCommandLineConfig],
      commandLineFromRaw: RawCommandLineConfig => CommandLineConfig,
      logger: (JsonConfig, RawCommandLineConfig) => RIO[Scope, Logger],
      telemetry: (JsonConfig, RawCommandLineConfig) => RIO[Scope, Telemetry],
  ) extends Builder7[Error, JsonConfig, RawCommandLineConfig, CommandLineConfig, Any](
        errorLogger,
        jsonConfig,
        parser,
        commandLineFromRaw,
        logger,
        telemetry,
        (_, _) => ZLayer.empty,
        EnvironmentTag[Any],
      ) {

    final def withoutEnv: Builder7[Error, JsonConfig, RawCommandLineConfig, CommandLineConfig, Any] = this

    final def withEnv[Env: EnvironmentTag](env: (JsonConfig, CommandLineConfig) => ZLayer[Scope, Error, Env]): Builder7[Error, JsonConfig, RawCommandLineConfig, CommandLineConfig, Env] =
      new Builder7(errorLogger, jsonConfig, parser, commandLineFromRaw, logger, telemetry, env, EnvironmentTag[Env])

  }

  // Needs Effect
  class Builder7[_Error, _JsonConfig, _RawCommandLineConfig, _CommandLineConfig, _Env](
      errorLogger: ErrorLogger[_Error],
      jsonConfig: JsonDecoder[_JsonConfig],
      parser: Parser[_RawCommandLineConfig],
      commandLineFromRaw: _RawCommandLineConfig => _CommandLineConfig,
      logger: (_JsonConfig, _RawCommandLineConfig) => RIO[Scope, Logger],
      telemetry: (_JsonConfig, _RawCommandLineConfig) => RIO[Scope, Telemetry],
      env: (_JsonConfig, _CommandLineConfig) => ZLayer[Scope, _Error, _Env],
      envTag: EnvironmentTag[_Env],
  ) { self =>

    final def implementRaw(effect: (_JsonConfig, _RawCommandLineConfig) => ZIO[_Env & Scope, _Error, Unit]): Executable.Single =
      new Executable.Single {

        override protected type JsonConfig = _JsonConfig
        override protected type CommandLineConfig = _RawCommandLineConfig
        override protected type Env = _Env
        override protected type Error = _Error

        override protected val commandLineParser: Parser[_RawCommandLineConfig] = self.parser
        override protected implicit val jsonConfigDecoder: JsonDecoder[_JsonConfig] = self.jsonConfig
        override protected implicit val errorLogger: ErrorLogger[_Error] = self.errorLogger
        override protected implicit val envTag: EnvironmentTag[_Env] = self.envTag

        override protected def logger(c1: _JsonConfig, c2: _RawCommandLineConfig): RIO[Scope, Logger] = self.logger(c1, c2)
        override protected def telemetry(c1: _JsonConfig, c2: _RawCommandLineConfig): RIO[Scope, Telemetry] = self.telemetry(c1, c2)
        override protected def myEnv(c1: _JsonConfig, c2: _RawCommandLineConfig): ZLayer[Scope, _Error, _Env] = self.env(c1, self.commandLineFromRaw(c2))

        override protected def run(c1: _JsonConfig, c2: _RawCommandLineConfig): ZIO[_Env & Scope, _Error, Unit] = effect(c1, c2)

      }

    final def implementJC(effect: (_JsonConfig, _CommandLineConfig) => ZIO[_Env & Scope, _Error, Unit]): Executable.Single =
      self.implementRaw { (c1, c2) => effect(c1, self.commandLineFromRaw(c2)) }

    final def implementC(effect: _CommandLineConfig => ZIO[_Env & Scope, _Error, Unit]): Executable.Single =
      self.implementRaw { (_, c2) => effect(self.commandLineFromRaw(c2)) }

    final def implement(effect: => ZIO[_Env & Scope, _Error, Unit]): Executable.Single =
      self.implementRaw { (_, _) => effect }

  }

}
