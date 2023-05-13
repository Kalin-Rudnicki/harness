package harness.zio

import cats.data.NonEmptyList
import cats.syntax.option.*
import harness.core.*
import java.io.PrintStream
import java.time.OffsetDateTime
import scala.collection.mutable
import zio.*
import zio.json.*

final case class Logger(
    sources: List[Logger.Source],
    defaultMinLogTolerance: Logger.LogLevel,
    defaultContext: Map[String, String],
    defaultColorMode: ColorMode,
) { self =>

  private val hasSourceWithLogToleranceOverride: Boolean = sources.exists(_.minLogTolerance.nonEmpty)

  def execute(event: Logger.Event): UIO[Unit] = {
    def handle(
        sourceMinLogTolerance: Logger.LogLevel,
        sourceColorMode: ColorMode,
        target: Logger.Target,
        logLevel: Option[Logger.LogLevel],
        event: Logger.Event,
        now: OffsetDateTime,
    ): UIO[Any] =
      event match {
        case Logger.Event.Compound(events) =>
          ZIO.foreachDiscard(events)(handle(sourceMinLogTolerance, sourceColorMode, target, logLevel, _, now))
        case Logger.Event.Output(context, message) =>
          val executedEvent = Logger.ExecutedEvent(logLevel, message, defaultContext ++ context, sourceColorMode, now)
          target.log(executedEvent)
        case e @ Logger.Event.AtLogLevel(logLevel, _) =>
          ZIO.when(logLevel.logPriority >= sourceMinLogTolerance.tolerancePriority) {
            handle(sourceMinLogTolerance, sourceColorMode, target, logLevel.some, e.event, now)
          }
      }

    def execOnSource(source: Logger.Source, now: OffsetDateTime): URIO[Scope, Any] =
      source.target.flatMap(handle(source.minLogTolerance.getOrElse(defaultMinLogTolerance), source.colorMode.getOrElse(defaultColorMode), _, None, event, now))

    (hasSourceWithLogToleranceOverride, event) match {
      case (false, Logger.Event.AtLogLevel(logLevel, _)) if logLevel.logPriority < defaultMinLogTolerance.tolerancePriority => ZIO.unit
      case _ =>
        ZIO.scoped {
          Clock.currentDateTime.flatMap { now =>
            ZIO.foreachParDiscard(sources)(execOnSource(_, now))
          }
        }
    }
  }

  def addContext(context: Map[String, String]): Logger =
    self.copy(defaultContext = defaultContext ++ context)

}
object Logger { self =>

  def default(
      sources: List[Logger.Source] = Logger.Source.stdOut(None, None) :: Nil,
      defaultMinLogTolerance: Logger.LogLevel = Logger.LogLevel.Info,
      defaultContext: List[(String, Any)] = Nil,
      defaultColorMode: ColorMode = ColorMode.Extended,
  ): Logger =
    Logger(
      sources = sources,
      defaultMinLogTolerance = defaultMinLogTolerance,
      defaultContext = defaultContext.map { (k, v) => (k, String.valueOf(v)) }.toMap,
      defaultColorMode = defaultColorMode,
    )

  // =====| API |=====

  def execute(event: => Event): URIO[Logger, Unit] = ZIO.service[Logger].flatMap(_.execute(event))

  object log {

    sealed class LogAtLevel(logLevel: LogLevel) {
      def apply(message: => Any, context: => (String, Any)*): URIO[Logger, Unit] =
        Logger.execute(Event.AtLogLevel(logLevel, () => Event.Output(context.toList.map { (k, v) => (k, String.valueOf(v)) }.toMap, String.valueOf(message))))
    }

    def apply(logLevel: LogLevel, message: => Any, context: => (String, Any)*): URIO[Logger, Unit] = LogAtLevel(logLevel)(message, context*)

    def apply(message: => Any, context: => (String, Any)*): URIO[Logger, Unit] = Logger.execute(Event.Output(context.toList.map { (k, v) => (k, String.valueOf(v)) }.toMap, String.valueOf(message)))

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
      def apply(error: => HError): URIO[Logger & RunMode & HError.UserMessage.IfHidden, Unit] =
        for {
          runMode <- ZIO.service[RunMode]
          ifHidden <- ZIO.service[HError.UserMessage.IfHidden]
          _ <-
            Logger.execute(
              Logger.Event.AtLogLevel(
                logLevel,
                () =>
                  Logger.Event.Compound(
                    error.toNel.toList.map(e => Logger.Event.Output(Map.empty, e.formatMessage(runMode, ifHidden))),
                  ),
              ),
            )
        } yield ()
    }

    def apply(logLevel: LogLevel, error: => HError): URIO[Logger & RunMode & HError.UserMessage.IfHidden, Unit] =
      LogAtLevel(logLevel)(error)

    def apply(error: => HError): URIO[Logger & RunMode & HError.UserMessage.IfHidden, Unit] =
      for {
        runMode <- ZIO.service[RunMode]
        ifHidden <- ZIO.service[HError.UserMessage.IfHidden]
        _ <-
          Logger.execute(
            Logger.Event.Compound(
              error.toNel.toList.map(e => Logger.Event.Output(Map.empty, e.formatMessage(runMode, ifHidden))),
            ),
          )
      } yield ()

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

  val getContext: URIO[Logger, Map[String, String]] =
    ZIO.serviceWith[Logger](_.defaultContext)

  def addContext[R, E, A](context: (String, Any)*)(effect: ZIO[R, E, A]): ZIO[R & Logger, E, A] =
    ZIO.serviceWithZIO[Logger] { logger =>
      effect.provideSomeLayer(ZLayer.succeed(logger.addContext(context.toList.map { (k, v) => (k, String.valueOf(v)) }.toMap)))
    }

  // =====| Types |=====

  final case class ExecutedEvent(
      logLevel: Option[LogLevel],
      message: String,
      context: Map[String, String],
      colorMode: ColorMode,
      dateTime: OffsetDateTime,
  ) {

    // TODO (KR) : Option to show `dateTime` in log message
    def formatted: String = {
      val msg: String =
        if (message.contains('\n')) message.replaceAll("\n", LogLevel.newLineIndent)
        else message
      // TODO (KR) : Colorization config
      val contextMsg =
        if (context.isEmpty) ""
        else s"${context.map { (k, v) => s"${colorMode.fgColorize(Color.Named.Cyan, k)}=${colorMode.fgColorize(Color.Named.Magenta, v)}" }.mkString(" ; ")}${LogLevel.newLineIndent}"
      s"[${logLevel.fold(LogLevel.emptyDisplayName)(_.colorizedDisplayName(colorMode))}]: $contextMsg$msg"
    }

    def toEncoded: ExecutedEvent.Encoded = ExecutedEvent.Encoded(logLevel, message, context, dateTime)

  }
  object ExecutedEvent {

    final case class Encoded(
        logLevel: Option[LogLevel],
        message: String,
        context: Map[String, String],
        dateTime: OffsetDateTime,
    )
    object Encoded {

      private implicit val logLevelJsonCodec: JsonCodec[LogLevel] = JsonCodec.fromHarnessStringEncoderAndDecoder

      implicit val jsonCodec: JsonCodec[Encoded] = DeriveJsonCodec.gen

    }

  }

  trait Target {
    def log(event: ExecutedEvent): UIO[Unit]
  }
  object Target {

    def fromPrintStream(name: String, printStream: PrintStream, eventToString: ExecutedEvent => String): Target =
      new Target {
        override def log(event: ExecutedEvent): UIO[Unit] = ZIO.hAttempt { printStream.println(eventToString(event)) }.orDie
      }

  }

  final case class Source(
      target: URIO[Scope, Target],
      minLogTolerance: Option[LogLevel],
      colorMode: Option[ColorMode],
  )
  object Source {

    def const(
        target: Target,
        minLogTolerance: Option[LogLevel],
        colorMode: Option[ColorMode],
    ): Source =
      Source(
        ZIO.succeed(target),
        minLogTolerance,
        colorMode,
      )

    def stdOut(
        minLogTolerance: Option[LogLevel],
        colorMode: Option[ColorMode],
    ): Source =
      Source.const(
        Target.fromPrintStream("StdOut", scala.Console.out, _.formatted),
        minLogTolerance,
        colorMode,
      )

    def stdOutJson(
        minLogTolerance: Option[LogLevel],
    ): Source =
      Source.const(
        Target.fromPrintStream("StdOut", scala.Console.out, _.toEncoded.toJson),
        minLogTolerance,
        ColorMode.Colorless.some,
      )

    def stringBuilder(
        sb: mutable.StringBuilder,
        minLogTolerance: Option[LogLevel],
        colorMode: Option[ColorMode],
    ): Source =
      Source.const(
        new Target {
          override def log(event: ExecutedEvent): UIO[Unit] = ZIO.succeed { sb.append(event.formatted); sb.append('\n') }
        },
        minLogTolerance,
        colorMode,
      )

  }

  sealed abstract class LogLevel(
      final val name: String,
      final val rawDisplayName: String,
      final val tolerancePriority: Int,
      final val logPriority: Int,
      final val extendedColor: Color,
      final val simpleColor: Color.Simple,
  ) {

    def this(
        name: String,
        rawDisplayName: String,
        priority: Int,
        extendedColor: Color,
        simpleColor: Color.Simple,
    ) =
      this(
        name = name,
        rawDisplayName = rawDisplayName,
        tolerancePriority = priority,
        logPriority = priority,
        extendedColor = extendedColor,
        simpleColor = simpleColor,
      )

    final lazy val displayName: String =
      s"$rawDisplayName${" " * (LogLevel.maxDisplayNameLength - rawDisplayName.length)}"

    private final lazy val colorModeDisplayNameMap: Map[ColorMode, String] =
      ColorMode.all.map { cm =>
        cm ->
          (cm.selectColor(extendedColor, simpleColor) match {
            case Color.Default => displayName
            case color         => s"${cm.fgANSI(color)}$displayName${cm.fgANSI(Color.Default)}"
          })
      }.toMap

    final def colorizedDisplayName(colorMode: ColorMode): String = colorModeDisplayNameMap(colorMode)

    override final def toString: String = rawDisplayName

  }
  object LogLevel {

    case object Never
        extends LogLevel(
          name = "Never",
          rawDisplayName = "NEVER",
          tolerancePriority = 10,
          logPriority = 0,
          extendedColor = Color.Default,
          simpleColor = Color.Default,
        )

    case object Trace
        extends LogLevel(
          name = "Trace",
          rawDisplayName = "TRACE",
          priority = 1,
          extendedColor = Color(0x30f2c2),
          simpleColor = Color.Named.Cyan,
        )

    case object Debug
        extends LogLevel(
          name = "Debug",
          rawDisplayName = "DEBUG",
          priority = 2,
          extendedColor = Color(0x0277bd),
          simpleColor = Color.Named.Cyan,
        )

    case object Detailed
        extends LogLevel(
          name = "Detailed",
          rawDisplayName = "DETLD",
          priority = 3,
          extendedColor = Color(0x66bb6a),
          simpleColor = Color.Named.Blue,
        )

    case object Info
        extends LogLevel(
          name = "Info",
          rawDisplayName = "INFO",
          priority = 4,
          extendedColor = Color(0x1b5e20),
          simpleColor = Color.Named.Green,
        )

    case object Important
        extends LogLevel(
          name = "Important",
          rawDisplayName = "IMPRT",
          priority = 5,
          extendedColor = Color(0x880e4f),
          simpleColor = Color.Named.Yellow,
        )

    case object Warning
        extends LogLevel(
          name = "Warning",
          rawDisplayName = "WARN",
          priority = 6,
          extendedColor = Color(0xffff00),
          simpleColor = Color.Named.Yellow,
        )

    case object Error
        extends LogLevel(
          name = "Error",
          rawDisplayName = "ERROR",
          priority = 7,
          extendedColor = Color(0xff3d00),
          simpleColor = Color.Named.Red,
        )

    case object Fatal
        extends LogLevel(
          name = "Fatal",
          rawDisplayName = "FATAL",
          priority = 8,
          extendedColor = Color(0xd50000),
          simpleColor = Color.Named.Red,
        )

    case object Always
        extends LogLevel(
          name = "Always",
          rawDisplayName = "ALWYS",
          priority = 9,
          extendedColor = Color.Default,
          simpleColor = Color.Default,
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
      allLevels.map(l => (l.rawDisplayName.toUpperCase, l)).toMap

    implicit val stringEncoder: StringEncoder[LogLevel] =
      _.rawDisplayName

    implicit val stringDecoder: StringDecoder[LogLevel] =
      StringDecoder.fromOptionF("LogLevel", str => nameMap.get(str.toUpperCase))

  }

  sealed trait Event
  object Event {

    final case class Compound(events: List[Event]) extends Event
    final case class Output(context: Map[String, String], message: String) extends Event
    final case class AtLogLevel(logLevel: LogLevel, private val _event: () => Event) extends Event {
      lazy val event: Event = _event()
    }

  }

}
