package harness.zio

import cats.syntax.option.*
import harness.core.*
import harness.zio.error.SysError
import scala.annotation.{nowarn, tailrec}
import scala.sys.process.*
import zio.*
import zio.json.*

trait Sys {

  def execute(cmd: Sys.Command, logLevels: Option[Sys.LogLevels]): ZIO[Logger, SysError, Int]
  def executeString0(cmd: Sys.Command, logLevels: Option[Sys.LogLevels]): ZIO[Logger, SysError, String]

  final def execute0(cmd: Sys.Command, logLevels: Option[Sys.LogLevels]): ZIO[Logger, SysError, Unit] =
    execute(cmd, logLevels).flatMap {
      case 0    => ZIO.unit
      case code => ZIO.fail(SysError.NonZeroExitCode(cmd, code))
    }

}
object Sys {

  final case class LogLevels(outLevel: Logger.LogLevel, errLevel: Logger.LogLevel)

  final case class Command(
      cmd: String,
      args: List[String],
      env: Map[String, String],
  ) { self =>

    lazy val cmdAndArgs: List[String] = cmd :: args

    def toProcess: ProcessBuilder = Process(cmdAndArgs, None, env.toList*)

    def :+(tail: List[String]): Command = Command(cmd, args ::: tail, env)

    def addEnv(env: Map[String, String]): Command = Command(cmd, args, self.env ++ env)
    def addEnv(env: (String, String)*): Command = Command(cmd, args, self.env ++ env.toMap)

    // --- Execute ---

    abstract class ExecuteBuilder[A](run: (Sys, Option[Sys.LogLevels]) => ZIO[Logger, SysError, A]) {
      def apply(logLevels: Option[Sys.LogLevels]): ZIO[Sys & Logger, SysError, A] = ZIO.serviceWithZIO[Sys] { run(_, logLevels) }
      def apply(outLevel: Logger.LogLevel = Logger.LogLevel.Info, errLevel: Logger.LogLevel = Logger.LogLevel.Error): ZIO[Sys & Logger, SysError, A] = apply(LogLevels(outLevel, errLevel).some)
      def simple: ZIO[Sys & Logger, SysError, A] = apply(None)
    }

    object execute extends ExecuteBuilder[Int](_.execute(self, _))
    object execute0 extends ExecuteBuilder[Unit](_.execute0(self, _))
    object executeString0 extends ExecuteBuilder[String](_.executeString0(self, _))

  }
  object Command {

    type Arg = String | Seq[String] | Option[Seq[String]]
    object Arg {

      @nowarn
      def toSeq(arg: Arg): Seq[String] = arg match
        case str: String            => str :: Nil
        case None                   => Nil
        case Some(seq: Seq[String]) => seq
        case seq: Seq[String]       => seq

    }

    def apply(cmd: String, args: Arg*): Command =
      new Command(
        cmd,
        args.toList.flatMap(Arg.toSeq),
        Map.empty,
      )

  }

  // =====| Live |=====

  def liveLayer(addCommandLogContext: Boolean): ULayer[Sys] = ZLayer.succeed { Sys.Live(addCommandLogContext) }

  final case class Live(addCommandLogContext: Boolean) extends Sys {

    private inline def makeLogger(logLevels: LogLevels): URIO[Logger, HarnessProcessLogger] =
      ZIO.runtime[Logger].map(HarnessProcessLogger(_, logLevels.outLevel, logLevels.errLevel))

    private inline def attempt[O](cmd: Command)(thunk: => O): IO[SysError.GenericError, O] =
      ZIO.attempt { thunk }.mapError(SysError.GenericError(cmd, _))

    private def doRun[O](cmd: Sys.Command, logLevels: Option[LogLevels])(
        withLogger: (ProcessBuilder, HarnessProcessLogger) => O,
        withoutLogger: ProcessBuilder => O,
    ): ZIO[Logger, SysError, O] = {
      val base =
        logLevels match {
          case Some(logLevels) =>
            for {
              logger <- makeLogger(logLevels)
              o <- attempt(cmd) { withLogger(cmd.toProcess, logger) }
            } yield o
          case None =>
            attempt(cmd) { withoutLogger(cmd.toProcess) }
        }

      if (addCommandLogContext) Logger.addContext("cmd" -> cmd.cmd) { base }
      else base
    }

    override def execute(cmd: Command, logLevels: Option[LogLevels]): ZIO[Logger, SysError, Int] =
      doRun(cmd, logLevels)(_.!<(_), _.!<)

    override def executeString0(cmd: Command, logLevels: Option[LogLevels]): ZIO[Logger, SysError, String] =
      doRun(cmd, logLevels)(_.!!<(_), _.!!<)

  }

  // =====| Unimplemented |=====

  val unimplementedLayer: ULayer[Sys] = ZLayer.succeed { Sys.Unimplemented }

  case object Unimplemented extends Sys {

    override def execute(cmd: Command, logLevels: Option[LogLevels]): ZIO[Logger, SysError, Int] =
      ZIO.fail(SysError.GenericError(cmd, new UnsupportedOperationException("Unimplemented - Sys.execute")))

    override def executeString0(cmd: Command, logLevels: Option[LogLevels]): ZIO[Logger, SysError, String] =
      ZIO.fail(SysError.GenericError(cmd, new UnsupportedOperationException("Unimplemented - Sys.executeString0")))

  }

  // =====| Process Logger |=====

  // TODO (KR) : support multi-line collapsing into a single log event, ex:
  // [INFO ]: ABC
  //        : DEF
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

      private val decodeLogLevel: Unapply[String, Logger.LogLevel] = s => Logger.LogLevel.stringDecoder.decode(s.trim).toOption

      private val levelPrefixRegex1 = "^\\[([A-Za-z]+)[ ]*]:?[ \t]*(.*)$".r
      private val levelPrefixRegex2 = "^([A-Za-z]+):[ \t]*(.*)$".r

      private val levelAndMessage: Unapply[String, (String, String)] = {
        case levelPrefixRegex1(level, message) => (level, message).some
        case levelPrefixRegex2(level, message) => (level, message).some
        case _                                 => None
      }

      private def makeEvent(level: Logger.LogLevel, context: Map[String, String], message: String): Logger.Event =
        Logger.Event.AtLogLevel(level, () => Logger.Event.Output(context, message))

      val all: List[MessageDecoder] =
        List(
          MessageDecoder.make { case (defaultLevel, harnessLoggerJson(event)) => makeEvent(event.logLevel.getOrElse(defaultLevel), event.context, event.message) },
          MessageDecoder.make { case (defaultLevel, levelAndMessage(_, harnessLoggerJson(event))) => makeEvent(event.logLevel.getOrElse(defaultLevel), event.context, event.message) },
          MessageDecoder.make { case (_, levelAndMessage(decodeLogLevel(level), message)) => makeEvent(level, Map.empty, message) },
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

}
