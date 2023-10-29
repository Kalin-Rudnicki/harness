package harness.zio

import cats.data.{EitherNel, NonEmptyList}
import cats.syntax.either.*
import harness.cli.*
import harness.core.*
import harness.zio.Config as Cfg
import scala.annotation.tailrec
import scala.util.matching.Regex
import zio.*
import zio.json.*
import zio.json.ast.Json

trait Executable { self =>

  def execute(args: IndexedArgs): SHTask[Any]

  final def apply(args: List[String]): UIO[ExitCode] = {
    val result: Executable.Result[(HTaskLayer[HarnessEnv], SHTask[Any])] =
      for {
        args <- Executable.parseIndexedArgs(args)
        (harnessArgs, executableArgs) = Executable.parseSplitArgs(args)
        harnessEnvLayer <- Executable.parseHarnessEnvLayer(harnessArgs)
        executableEffect = self.execute(executableArgs)
      } yield (harnessEnvLayer, executableEffect)

    val execute: HRIO[Logger, ExitCode] =
      result match {
        case Executable.Result.Success((layer, effect)) =>
          effect.collapseCause
            .tapError(Logger.logHErrorMessageWithTrace.debug(_))
            .dumpErrorsAndContinue(Logger.LogLevel.Fatal)
            .provideLayer(layer)
            .map {
              case Some(_) => ExitCode.success
              case None    => ExitCode.failure
            }
        case Executable.Result.Help(message) =>
          Logger.log.info(message).as(ExitCode.success)
        case Executable.Result.Fail(error) =>
          Logger.logHErrorMessage.fatal(error).as(ExitCode.failure)
      }

    execute.collapseCause
      .catchAll(Logger.logHErrorMessageWithTrace.fatal(_).as(ExitCode.failure))
      .provideLayer(ZLayer.succeed(Logger.default()))
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
      configStrings: List[Config.ConfigString],
  )
  private object Config {

    sealed trait ConfigString
    object ConfigString {
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

      private val jarResRegex: Regex = "^jar:(.*)$".r
      private val fileResRegex: Regex = "^file:(.*)$".r
      private val jsonPathRegex: Regex = "^json:([A-Za-z_0-9]+(?:\\.[A-Za-z_0-9]+)*):(.*)$".r
      private val jsonRegex: Regex = "^json:(.*)$".r
      implicit val stringDecoder: StringDecoder[ConfigString] =
        StringDecoder.string.flatMap {
          case fileResRegex(path)        => ConfigString.FilePath(path).asRight
          case jarResRegex(path)         => ConfigString.JarResPath(path).asRight
          case jsonPathRegex(nest, json) => ConfigString.JsonConfig(nestJson(nest, parseJson(json))).asRight
          case jsonRegex(json)           => ConfigString.JsonConfig(parseJson(json)).asRight
          case _                         => "does not match a valid regex".leftNel
        }

    }

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
        ) &&
      Parser.values.list[Config.ConfigString](
        LongName.unsafe("config-path"),
        Defaultable.Some(ShortName.unsafe('C')),
        helpHint = List(
          "jar:path/to/jar/res",
          "file:path/to/file",
          """json:{ "js": { "path": "some-json" } }""",
          """json:js.path:"some-json"""",
        ),
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

  private def loadConfig(cfg: Config.ConfigString): HRIO[FileSystem, Cfg] =
    cfg match {
      case Config.ConfigString.FilePath(path)   => Cfg.fromPathString(path)
      case Config.ConfigString.JarResPath(path) => Cfg.fromJarResource(path)
      case Config.ConfigString.JsonConfig(json) => ZIO.succeed(Cfg.fromJson(json))
    }

  private def loadConfigs(configStrings: List[Config.ConfigString]): HRLayer[FileSystem, harness.zio.Config] =
    ZLayer.fromZIO { ZIO.foreach(configStrings)(loadConfig).map(harness.zio.Config.flatten) }

  private def parseHarnessEnvLayer(args: IndexedArgs): Result[HTaskLayer[HarnessEnv]] =
    finalizedResultToExecutableResult(Config.parser.finalized.parse(args)).map { config =>
      val logger: Logger =
        Logger.default(
          sources = (if (config.logJson) Logger.Source.stdOutJson(None) else Logger.Source.stdOut(None, None)) :: Nil,
          defaultMinLogTolerance = config.minLogTolerance,
          defaultColorMode = config.colorMode,
        )

      ZLayer.make[HarnessEnv](
        ZLayer.succeed(logger),
        ZLayer.succeed(Telemetry.log),
        ZLayer.succeed(config.runMode),
        ZLayer.succeed(HError.UserMessage.IfHidden.default),
        FileSystem.liveLayer,
        loadConfigs(config.configStrings),
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
