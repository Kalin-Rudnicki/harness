package harness.zio

import cats.data.NonEmptyList
import cats.syntax.option.*
import harness.core.*
import harness.zio.error.SysError
import scala.annotation.tailrec
import scala.sys.process.*
import zio.*
import zio.json.*

object Sys {

  final case class HarnessProcessLogger(runtime: Runtime[Logger], outLevel: Logger.LogLevel, errLevel: Logger.LogLevel) extends ProcessLogger {

    private def run(defaultLevel: Logger.LogLevel, message: String): Unit =
      Unsafe.unsafely { runtime.unsafe.run(Logger.execute(HarnessProcessLogger.MessageDecoder.decode(defaultLevel, message, HarnessProcessLogger.MessageDecoder.all))).getOrThrow() }

    override def out(s: => String): Unit = run(outLevel, s)

    override def err(s: => String): Unit = run(errLevel, s)

    override def buffer[T](f: => T): T = f

  }
  object HarnessProcessLogger {

    trait MessageDecoder {
      def decode(defaultLevel: Logger.LogLevel, message: String): Option[Logger.Event]
    }
    object MessageDecoder {

      def make(f: PartialFunction[(Logger.LogLevel, String), Logger.Event]): MessageDecoder =
        (l, m) => f.lift((l, m))

      private def decodeJson[A: JsonDecoder]: Unapply[String, A] = _.fromJson[A].toOption

      private val harnessLoggerJson: Unapply[String, Logger.ExecutedEvent] = decodeJson

      private val decodeLogLevel: Unapply[String, Logger.LogLevel] = Logger.LogLevel.stringDecoder.decode(_).toOption

      private val levelPrefixRegex = "^(\\[[A-Za-z]+])[ \t]*(.*)$".r

      private def makeEvent(level: Logger.LogLevel, context: Map[String, String], message: String): Logger.Event =
        Logger.Event.AtLogLevel(level, () => Logger.Event.Output(context, message))

      val all: List[MessageDecoder] =
        List(
          MessageDecoder.make { case (defaultLevel, harnessLoggerJson(event)) => makeEvent(event.logLevel.getOrElse(defaultLevel), event.context, event.message) },
          MessageDecoder.make { case (defaultLevel, levelPrefixRegex(_, harnessLoggerJson(event))) => makeEvent(event.logLevel.getOrElse(defaultLevel), event.context, event.message) },
          MessageDecoder.make { case (_, levelPrefixRegex(decodeLogLevel(level), message)) => makeEvent(level, Map.empty, message) },
          // TODO (KR) : add decoders for SLF4J json
        )

      @tailrec
      def decode(defaultLevel: Logger.LogLevel, message: String, decoders: List[MessageDecoder]): Logger.Event =
        decoders match {
          case head :: tail =>
            head.decode(defaultLevel, message) match {
              case Some(event) => event
              case None        => decode(defaultLevel, message, tail)
            }
          case Nil => makeEvent(defaultLevel, Map.empty, message)
        }

    }

  }

  final case class Command(cmdAndArgs: NonEmptyList[String]) {

    def sudo: Command = Command("sudo" :: cmdAndArgs)
    def sudoIf(cond: Boolean): Command = if (cond) this.sudo else this
    def :+(tail: List[String]): Command = Command(cmdAndArgs ++ tail)

    def show: String = cmdAndArgs.toList.map(_.unesc).mkString(" ")

  }
  object Command {
    def apply(cmd: String, args: String*): Command =
      new Command(NonEmptyList(cmd, args.toList))
    def apply(cmd: String, arg0: Option[List[String]], argN: Option[List[String]]*): Command =
      new Command(NonEmptyList(cmd, (arg0 :: argN.toList).flatMap(_.toList.flatten)))
  }

  abstract class CmdBuilder[O](private[Sys] val run: (ProcessBuilder, Option[HarnessProcessLogger], Command) => IO[SysError, O]) { self =>

    private final def makeRun(envVars: List[(String, String)], logger: Option[HarnessProcessLogger], cmd: Command): IO[SysError, O] =
      self.run(Process(cmd.cmdAndArgs.toList, None, envVars*), logger, cmd)

    private final def makeLogger(outLevel: Logger.LogLevel, errLevel: Logger.LogLevel): URIO[Logger, HarnessProcessLogger] =
      ZIO.runtime[Logger].map(HarnessProcessLogger(_, outLevel, errLevel))

    final def runComplex(
        envVars: Map[String, String] = Map.empty,
        outLevel: Logger.LogLevel = Logger.LogLevel.Info,
        errLevel: Logger.LogLevel = Logger.LogLevel.Error,
    )(cmd: Command): ZIO[Logger, SysError, O] =
      for {
        logger <- makeLogger(outLevel, errLevel)
        res <- makeRun(envVars.toList, logger.some, cmd)
      } yield res

    final def runSimple(cmd: Command): IO[SysError, O] = makeRun(Nil, None, cmd)
    final def runSimple(cmdAndArgs: NonEmptyList[String]): IO[SysError, O] = runSimple(Command(cmdAndArgs))
    final def runSimple(cmd: String, args: String*): IO[SysError, O] = runSimple(Command(NonEmptyList(cmd, args.toList)))

  }

  private def doRun[O](process: ProcessBuilder, logger: Option[HarnessProcessLogger], command: Command)(
      withLogger: (ProcessBuilder, HarnessProcessLogger) => O,
      withoutLogger: ProcessBuilder => O,
  ): IO[SysError, O] =
    ZIO
      .attempt {
        logger match {
          case Some(logger) => withLogger(process, logger)
          case None         => withoutLogger(process)
        }
      }
      .mapError(SysError.GenericError(command, _))

  object execute extends CmdBuilder[Int](doRun(_, _, _)(_.!<(_), _.!<))

  object execute0
      extends CmdBuilder[Unit]((process, logger, cmdAndArgs) =>
        Sys.execute
          .run(process, logger, cmdAndArgs)
          .flatMap {
            case 0    => ZIO.unit
            case code => ZIO.fail(SysError.NonZeroExitCode(cmdAndArgs, code))
          },
      )

  object executeString0 extends CmdBuilder[String](doRun(_, _, _)(_.!!<(_), _.!!<))

}
