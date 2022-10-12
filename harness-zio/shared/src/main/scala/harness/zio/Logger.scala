package harness.zio

import cats.data.NonEmptyList
import cats.syntax.option.*
import harness.core.*
import java.io.PrintStream
import scala.collection.mutable
import zio.*

final case class Logger(
    sources: List[Logger.Source],
    defaultMinLogTolerance: Logger.LogLevel,
    defaultContext: List[(String, Any)],
    colorize: Boolean, // TODO (KR) : Make this source-specific
) { self =>

  def execute(event: Logger.Event): UIO[Unit] = {
    def handle(
        sourceMinLogTolerance: Logger.LogLevel,
        target: Logger.Target,
        logLevel: Option[Logger.LogLevel],
        event: Logger.Event,
    ): UIO[Any] =
      event match {
        case Logger.Event.Compound(events) =>
          ZIO.foreachDiscard(events)(handle(sourceMinLogTolerance, target, logLevel, _))
        case Logger.Event.Output(context, message) =>
          target.log(Logger.formatMessage(logLevel, message, defaultContext ::: context, colorize))
        case e @ Logger.Event.AtLogLevel(logLevel, _) =>
          ZIO.when(logLevel.logPriority >= sourceMinLogTolerance.logPriority) {
            handle(sourceMinLogTolerance, target, logLevel.some, e.event)
          }
      }

    def execOnSource(source: Logger.Source): URIO[Scope, Any] =
      source.target.flatMap(handle(source.minLogTolerance.getOrElse(defaultMinLogTolerance), _, None, event))

    ZIO.scoped {
      ZIO.foreachParDiscard(sources)(execOnSource)
    }
  }

  def addContext(context: List[(String, Any)]): Logger =
    self.copy(defaultContext = defaultContext ::: context)

}
object Logger { self =>

  def default(
      sources: List[Logger.Source] = Logger.Source.stdOut(None) :: Nil,
      defaultMinLogTolerance: Logger.LogLevel = Logger.LogLevel.Info,
      defaultContext: List[(String, Any)] = Nil,
      colorize: Boolean = true,
  ): Logger =
    Logger(
      sources = sources,
      defaultMinLogTolerance = defaultMinLogTolerance,
      defaultContext = defaultContext,
      colorize = colorize,
    )

  // =====| API |=====

  def execute(event: => Event): URIO[Logger, Unit] = ZIO.service[Logger].flatMap(_.execute(event))

  object log {

    sealed class LogAtLevel(logLevel: LogLevel) {
      def apply(message: => Any, context: => (String, Any)*): URIO[Logger, Unit] =
        Logger.execute(Event.AtLogLevel(logLevel, () => Event.Output(context.toList, message)))
    }

    def apply(logLevel: LogLevel, message: => Any, context: => (String, Any)*): URIO[Logger, Unit] = LogAtLevel(logLevel)(message, context*)

    def apply(message: => Any, context: => (String, Any)*): URIO[Logger, Unit] = Logger.execute(Event.Output(context.toList, message))

    object never extends LogAtLevel(LogLevel.Never)
    object trace extends LogAtLevel(LogLevel.Trace)
    object debug extends LogAtLevel(LogLevel.Debug)
    object detailed extends LogAtLevel(LogLevel.Detailed)
    object info extends LogAtLevel(LogLevel.Info)
    object important extends LogAtLevel(LogLevel.Important)
    object warning extends LogAtLevel(LogLevel.Warning)
    object error extends LogAtLevel(LogLevel.Error)
    object fatal extends LogAtLevel(LogLevel.Fatal)
    object always extends LogAtLevel(LogLevel.Always)

  }

  object logHError {

    sealed class LogAtLevel(logLevel: LogLevel) {
      def apply(error: => HError): URIO[Logger & RunMode, Unit] =
        RunMode.get.flatMap { runMode =>
          Logger.log(logLevel, runMode.formatError(error))
        }
    }

    def apply(logLevel: LogLevel, error: => HError): URIO[Logger & RunMode, Unit] = LogAtLevel(logLevel)(error)

    def apply(error: => HError): URIO[Logger & RunMode, Unit] =
      RunMode.get.flatMap { runMode =>
        Logger.log(runMode.formatError(error))
      }

    object never extends LogAtLevel(LogLevel.Never)
    object trace extends LogAtLevel(LogLevel.Trace)
    object debug extends LogAtLevel(LogLevel.Debug)
    object detailed extends LogAtLevel(LogLevel.Detailed)
    object info extends LogAtLevel(LogLevel.Info)
    object important extends LogAtLevel(LogLevel.Important)
    object warning extends LogAtLevel(LogLevel.Warning)
    object error extends LogAtLevel(LogLevel.Error)
    object fatal extends LogAtLevel(LogLevel.Fatal)
    object always extends LogAtLevel(LogLevel.Always)

  }

  def addContext[R, E, A](context: (String, Any)*)(effect: ZIO[R, E, A]): ZIO[R & Logger, E, A] =
    ZIO.service[Logger].flatMap { logger =>
      effect.provideSomeLayer(ZLayer.succeed(logger.addContext(context.toList)))
    }

  // =====| Types |=====

  trait Target {
    def log(string: String): UIO[Unit]
  }
  object Target {

    def fromPrintStream(name: String, printStream: PrintStream): Target =
      new Target {
        override def log(string: String): UIO[Unit] = ZIO.hAttempt(s"Unable to log to PrintStream: $name") { printStream.println(string) }.orDieH
      }

  }

  final case class Source(
      target: URIO[Scope, Target],
      minLogTolerance: Option[LogLevel],
  )
  object Source {

    def const(
        target: Target,
        minLogTolerance: Option[LogLevel],
    ): Source =
      Source(
        ZIO.succeed(target),
        minLogTolerance,
      )

    def stdOut(
        minLogTolerance: Option[LogLevel],
    ): Source =
      Source.const(
        Target.fromPrintStream("StdOut", scala.Console.out),
        minLogTolerance,
      )

    def stringBuilder(
        sb: mutable.StringBuilder,
        minLogTolerance: Option[LogLevel],
    ): Source =
      Source.const(
        new Target {
          override def log(string: String): UIO[Unit] = ZIO.succeed { sb.append(string); sb.append('\n') }
        },
        minLogTolerance,
      )

  }

  sealed abstract class LogLevel(
      final val name: String,
      final val rawDisplayName: String,
      final val tolerancePriority: Int,
      final val logPriority: Int,
  ) {

    def this(
        name: String,
        rawDisplayName: String,
        priority: Int,
    ) =
      this(
        name = name,
        rawDisplayName = rawDisplayName,
        tolerancePriority = priority,
        logPriority = priority,
      )

    final lazy val displayName: String =
      s"$rawDisplayName${" " * (LogLevel.maxDisplayNameLength - rawDisplayName.length)}"

    override final def toString: String = rawDisplayName

  }
  object LogLevel {

    case object Never
        extends LogLevel(
          name = "Never",
          rawDisplayName = "NEVER",
          tolerancePriority = 10,
          logPriority = 0,
        )

    case object Trace
        extends LogLevel(
          name = "Trace",
          rawDisplayName = "TRACE",
          priority = 1,
        )

    case object Debug
        extends LogLevel(
          name = "Debug",
          rawDisplayName = "DEBUG",
          priority = 2,
        )

    case object Detailed
        extends LogLevel(
          name = "Detailed",
          rawDisplayName = "DETLD",
          priority = 3,
        )

    case object Info
        extends LogLevel(
          name = "Info",
          rawDisplayName = "INFO",
          priority = 4,
        )

    case object Important
        extends LogLevel(
          name = "Important",
          rawDisplayName = "IMPRT",
          priority = 5,
        )

    case object Warning
        extends LogLevel(
          name = "Warning",
          rawDisplayName = "WARN",
          priority = 6,
        )

    case object Error
        extends LogLevel(
          name = "Error",
          rawDisplayName = "ERROR",
          priority = 7,
        )

    case object Fatal
        extends LogLevel(
          name = "Fatal",
          rawDisplayName = "FATAL",
          priority = 8,
        )

    case object Always
        extends LogLevel(
          name = "Always",
          rawDisplayName = "ALWYS",
          priority = 9,
        )

    val allLevels: List[LogLevel] =
      List(
        Never,
        Trace,
        Debug,
        Detailed,
        Info,
        Important,
        Warning,
        Error,
        Fatal,
        Always,
      )

    val maxDisplayNameLength: Int =
      allLevels.map(_.rawDisplayName.length).max

    val emptyDisplayName: String =
      " " * maxDisplayNameLength

    val newLineIndent: String =
      s"\n $emptyDisplayName : "

    val nameMap: Map[String, LogLevel] =
      allLevels.map(l => (l.rawDisplayName, l)).toMap

    implicit val stringDecoder: StringDecoder[LogLevel] =
      StringDecoder.fromOptionF("LogLevel", str => nameMap.get(str.toUpperCase))

  }

  sealed trait Event
  object Event {

    final case class Compound(events: List[Event]) extends Event
    final case class Output(context: List[(String, Any)], message: Any) extends Event
    final case class AtLogLevel(logLevel: LogLevel, private val _event: () => Event) extends Event {
      lazy val event: Event = _event()
    }

  }

  // =====| Helpers |=====

  def formatMessage(
      logLevel: Option[LogLevel],
      message: Any,
      context: List[(String, Any)],
      colorize: Boolean,
  ): String = {
    import scala.Console.{MAGENTA, CYAN, RESET}
    val tmpMsg: String = message.toString
    val msg: String =
      if (tmpMsg.contains('\n')) tmpMsg.replaceAll("\n", LogLevel.newLineIndent)
      else tmpMsg
    // TODO (KR) : Colorization config
    val contextMsg =
      if (context.isEmpty) ""
      else if (colorize) s"${context.map { (k, v) => s"$CYAN$k$RESET=$MAGENTA$v$RESET" }.mkString(" ; ")}${LogLevel.newLineIndent}"
      else s"${context.map { (k, v) => s"$k=$v" }.mkString(" ; ")}${LogLevel.newLineIndent}"
    s"[${logLevel.fold(LogLevel.emptyDisplayName)(_.displayName)}]: $contextMsg$msg"
  }

}
