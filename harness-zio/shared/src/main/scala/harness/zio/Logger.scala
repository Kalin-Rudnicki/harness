package harness.zio

import cats.syntax.option.*
import java.io.PrintStream
import scala.collection.mutable
import zio.*

final class Logger(
    sources: List[Logger.Source],
) {

  def execute(event: Logger.Event): UIO[Unit] = {
    def handle(
        source: Logger.Source,
        target: Logger.Target,
        logLevel: Option[Logger.LogLevel],
        event: Logger.Event,
    ): UIO[Any] =
      event match {
        case Logger.Event.Compound(events) => ZIO.foreachDiscard(events)(handle(source, target, logLevel, _))
        case Logger.Event.Output(message)  => target.log(Logger.formatMessage(logLevel, message))
        case e @ Logger.Event.AtLogLevel(logLevel, _) =>
          ZIO.when(logLevel.logPriority >= source.minLogTolerance.tolerancePriority && logLevel.logPriority <= source.maxLogTolerance.logPriority) {
            handle(source, target, logLevel.some, e.event)
          }
      }

    def execOnSource(source: Logger.Source): URIO[Scope, Any] =
      source.target.flatMap(handle(source, _, None, event))

    ZIO.scoped {
      ZIO.foreachParDiscard(sources)(execOnSource)
    }
  }

}
object Logger { self =>

  // =====| API |=====

  def execute(event: => Event): URIO[Logger, Unit] = ZIO.service[Logger].flatMap(_.execute(event))

  object log {

    sealed abstract class LogAtLevel(logLevel: LogLevel) {
      def apply(message: => Any): URIO[Logger, Unit] = Logger.execute(Event.AtLogLevel(logLevel, () => Event.Output(message)))
    }

    def apply(logLevel: LogLevel, message: => Any): URIO[Logger, Unit] = Logger.execute(Event.AtLogLevel(logLevel, () => Event.Output(message)))
    def apply(message: => Any): URIO[Logger, Unit] = Logger.execute(Event.Output(message))

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
      minLogTolerance: LogLevel,
      maxLogTolerance: LogLevel,
  )
  object Source {

    def const(
        target: Target,
        minLogTolerance: LogLevel,
        maxLogTolerance: LogLevel = LogLevel.Always,
    ): Source =
      Source(
        ZIO.succeed(target),
        minLogTolerance,
        maxLogTolerance,
      )

    def stdOut(
        minLogTolerance: LogLevel,
        maxLogTolerance: LogLevel = LogLevel.Always,
    ): Source =
      Source.const(
        Target.fromPrintStream("StdOut", scala.Console.out),
        minLogTolerance,
        maxLogTolerance,
      )

    def stringBuilder(
        sb: mutable.StringBuilder,
        minLogTolerance: LogLevel,
        maxLogTolerance: LogLevel = LogLevel.Always,
    ): Source =
      Source.const(
        new Target {
          override def log(string: String): UIO[Unit] = ZIO.succeed { sb.append(string); sb.append('\n') }
        },
        minLogTolerance,
        maxLogTolerance,
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
      s"${rawDisplayName}${" " * (LogLevel.maxDisplayNameLength - rawDisplayName.length)}"

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

  }

  sealed trait Event
  object Event {

    final case class Compound(events: List[Event]) extends Event
    final case class Output(message: Any) extends Event
    final case class AtLogLevel(logLevel: LogLevel, private val _event: () => Event) extends Event {
      final lazy val event: Event = _event()
    }

  }

  // =====| Helpers |=====

  def formatMessage(
      logLevel: Option[LogLevel],
      message: Any,
  ): String = {
    val tmpMsg: String = message.toString
    val msg: String =
      if (tmpMsg.contains('\n')) tmpMsg.replaceAll("\n", LogLevel.newLineIndent)
      else tmpMsg
    s"[${logLevel.fold(LogLevel.emptyDisplayName)(_.displayName)}]: $msg"
  }

}
