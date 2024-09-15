package harness.zio

import cats.data.NonEmptyList
import cats.syntax.option.*
import harness.core.*
import harness.zio.json.*
import java.io.PrintStream
import java.time.Instant
import scala.annotation.nowarn
import scala.collection.mutable
import zio.*
import zio.json.*
import zio.json.ast.Json

final case class Logger(
    sources: Chunk[Logger.Source],
    context: Chunk[(String, String)],
    defaultMinLogTolerance: Logger.LogLevel,
    forwardToZio: Boolean,
) {

  def execute(event: Logger.Event): UIO[Unit] =
    Clock.instant.flatMap { now =>
      ZIO.whenDiscard(forwardToZio)(execZIOLogger(event)) *>
        ZIO.scoped { ZIO.foreachParDiscard(sources)(execOnSource(event, _, now)) }
    }

  def withSources(sources: Chunk[Logger.Source]): Logger = copy(sources = sources)
  def addSources(sources: Chunk[Logger.Source]): Logger = copy(sources = this.sources ++ sources)

  def withContext(context: Chunk[(String, String)]): Logger = copy(context = context)
  def addContext(context: Chunk[(String, String)]): Logger = copy(context = this.context ++ context)

  def withDefaultMinLogTolerance(defaultMinLogTolerance: Logger.LogLevel): Logger = copy(defaultMinLogTolerance = defaultMinLogTolerance)

  // =====|  |=====

  private def execZIOLogger(e: Logger.Event): UIO[Unit] =
    for {
      runtime <- ZIO.runtime[Any]
      fiberId <- ZIO.fiberId
      loggers <- ZIO.loggers

      zioLogAnnotations <- ZIO.logAnnotations
      zioLogSpans <- ZIO.logSpans

      _ <- ZIO.foreachParDiscard(loggers) { logger =>
        logger(
          e.trace.toZio,
          fiberId,
          e.level.getOrElse(Logger.LogLevel.Always).zioLogLevel,
          e._message,
          e.cause.toZio,
          runtime.fiberRefs,
          zioLogSpans,
          zioLogAnnotations ++ (this.context ++ e.context).toMap,
        )

        ZIO.unit
      }
    } yield ()

  private def handle(
      sourceMinLogTolerance: Logger.LogLevel,
      target: Logger.Target,
      e: Logger.Event,
      now: Instant,
  ): UIO[Unit] =
    e match {
      case Logger.Event(Some(logLevel), _, _, _, _, _) if logLevel.logPriority < sourceMinLogTolerance.tolerancePriority =>
        ZIO.unit
      case _ =>
        target.log(Logger.ExecutedEvent(e.level, e.message, this.context ++ e.context, e.cause, e.trace, e.stackTrace, now))
    }

  private def execOnSource(event: Logger.Event, source: Logger.Source, now: Instant): URIO[Scope, Unit] =
    source.target.flatMap { handle(source.minLogTolerance.getOrElse(defaultMinLogTolerance), _, event, now) }

  override def toString: String =
    s"""Logger[level=$defaultMinLogTolerance]:
       |  sources:${sources.map(s => s"\n    - $s").mkString}
       |  context:${context.map { case (k, v) => s"\n    $k: $v" }.mkString}""".stripMargin

}
object Logger {

  // =====| Impl |=====

  // TODO (KR) : Support Patching
  private[zio] val loggerRef: FiberRef[Logger] =
    Unsafe.unsafely {
      FiberRef.unsafe.make[Logger](
        Logger(
          sources = Chunk.empty,
          context = Chunk.empty,
          defaultMinLogTolerance = Logger.LogLevel.Info,
          forwardToZio = true,
        ),
        identity,
        (_, child) => child,
      )
    }

  // =====| API |=====

  def current: UIO[Logger] = Logger.loggerRef.get

  // --- Modify ---

  def withLogger(f: Logger => Logger): FiberRefModification =
    Logger.loggerRef.modification.update(f)
  def withLogger(logger: Logger): FiberRefModification =
    Logger.loggerRef.modification.set(logger)

  def withSources(sources: Chunk[Logger.Source]): FiberRefModification =
    Logger.loggerRef.modification.update(_.withSources(sources))
  def withSources(sources: Logger.Source*): FiberRefModification =
    Logger.loggerRef.modification.update(_.withSources(Chunk.fromIterable(sources)))
  def addSources(sources: Chunk[Logger.Source]): FiberRefModification =
    Logger.loggerRef.modification.update(_.addSources(sources))
  def withoutStdOutSources: FiberRefModification =
    Logger.withLogger { logger => logger.withSources(logger.sources.filterNot(s => Logger.Source.names.allStdOut.contains(s.name))) }

  def withContext(context: Chunk[(String, String)]): FiberRefModification =
    Logger.loggerRef.modification.update(_.withContext(context))
  def withContext(context: (String, Any)*): FiberRefModification =
    Logger.loggerRef.modification.update(_.withContext(Logger.LogContext(context)))
  def addContext(context: Chunk[(String, String)]): FiberRefModification =
    Logger.loggerRef.modification.update(_.addContext(context))
  def addContext(context: (String, Any)*): FiberRefModification =
    Logger.loggerRef.modification.update(_.addContext(Logger.LogContext(context)))

  object withLevel extends WithLogLevel[FiberRefModification](level => Logger.loggerRef.modification.update(_.withDefaultMinLogTolerance(level)))
  object withDefaultMinLogTolerance extends WithLogLevel[FiberRefModification](level => Logger.loggerRef.modification.update(_.withDefaultMinLogTolerance(level)))

  def withForwardToZio(forwardToZio: Boolean): FiberRefModification =
    Logger.loggerRef.modification.update(_.copy(forwardToZio = forwardToZio))

  def resetToZio: FiberRefModification =
    Logger.loggerRef.modification.update(_.copy(sources = Chunk.empty, forwardToZio = true))

  // --- Execution ---

  def execute(event: Logger.Event): UIO[Unit] =
    Logger.loggerRef.getWith(_.execute(event))

  object log {

    sealed class LogAtLevel(logLevel: Logger.LogLevel) {
      def apply(message: => Any, context: => (String, Any)*)(implicit trace: zio.Trace): UIO[Unit] =
        Logger.execute(Logger.Event(logLevel, String.valueOf(message), Logger.LogContext(context), Cause.Empty, Logger.Trace.fromZio(trace), None))
    }

    def apply(logLevel: Logger.LogLevel): LogAtLevel = new LogAtLevel(logLevel)

    def apply(message: => Any, context: => (String, Any)*)(implicit trace: zio.Trace): UIO[Unit] =
      Logger.execute(Logger.Event(String.valueOf(message), Logger.LogContext(context), Cause.Empty, Logger.Trace.fromZio(trace), None))

    object never extends LogAtLevel(Logger.LogLevel.Never)
    object trace extends LogAtLevel(Logger.LogLevel.Trace)
    object debug extends LogAtLevel(Logger.LogLevel.Debug)
    object detailed extends LogAtLevel(Logger.LogLevel.Detailed)
    object info extends LogAtLevel(Logger.LogLevel.Info)
    object important extends LogAtLevel(Logger.LogLevel.Important)
    object warning extends LogAtLevel(Logger.LogLevel.Warning)
    object error extends LogAtLevel(Logger.LogLevel.Error)
    object fatal extends LogAtLevel(Logger.LogLevel.Fatal)
    object always extends LogAtLevel(Logger.LogLevel.Always)

  }

  object logStack {

    sealed class LogAtLevel(logLevel: Logger.LogLevel) {
      def apply(message: => Any, context: => (String, Any)*)(implicit trace: zio.Trace): UIO[Unit] =
        ZIO.stackTrace.flatMap { stackTrace =>
          Logger.execute(Logger.Event(logLevel, String.valueOf(message), Logger.LogContext(context), Cause.Empty, Logger.Trace.fromZio(trace), Logger.StackTrace.fromZio(stackTrace).some))
        }
    }

    def apply(logLevel: Logger.LogLevel): LogAtLevel = new LogAtLevel(logLevel)

    def apply(message: => Any, context: => (String, Any)*)(implicit trace: zio.Trace): UIO[Unit] =
      ZIO.stackTrace.flatMap { stackTrace =>
        Logger.execute(Logger.Event(String.valueOf(message), Logger.LogContext(context), Cause.Empty, Logger.Trace.fromZio(trace), Logger.StackTrace.fromZio(stackTrace).some))
      }

    object never extends LogAtLevel(Logger.LogLevel.Never)
    object trace extends LogAtLevel(Logger.LogLevel.Trace)
    object debug extends LogAtLevel(Logger.LogLevel.Debug)
    object detailed extends LogAtLevel(Logger.LogLevel.Detailed)
    object info extends LogAtLevel(Logger.LogLevel.Info)
    object important extends LogAtLevel(Logger.LogLevel.Important)
    object warning extends LogAtLevel(Logger.LogLevel.Warning)
    object error extends LogAtLevel(Logger.LogLevel.Error)
    object fatal extends LogAtLevel(Logger.LogLevel.Fatal)
    object always extends LogAtLevel(Logger.LogLevel.Always)

  }

  object logCause {

    def apply[E](causeLevel: Option[Logger.LogLevel], message: => Any, cause: zio.Cause[E], context: => (String, Any)*)(implicit trace: zio.Trace, errorLogger: ErrorLogger[E]): UIO[Unit] =
      cause.failures.map(errorLogger.logLevel).maxByOption(_.logPriority).orElse(causeLevel) match
        case Some(level) => Logger.execute(Logger.Event(level, String.valueOf(message), Logger.LogContext(context), Logger.Cause.fromZio(cause, errorLogger), Logger.Trace.fromZio(trace), None))
        case None        => Logger.execute(Logger.Event(String.valueOf(message), Logger.LogContext(context), Logger.Cause.fromZio(cause, errorLogger), Logger.Trace.fromZio(trace), None))

    sealed class LogCauseAtLevel(causeLevel: Logger.LogLevel) {

      def apply[E](cause: zio.Cause[E], context: => (String, Any)*)(implicit trace: zio.Trace, errorLogger: ErrorLogger[E]): UIO[Unit] =
        Logger.logCause(causeLevel.some, "", cause, context*)

      def apply[E](message: => Any, cause: zio.Cause[E], context: => (String, Any)*)(implicit trace: zio.Trace, errorLogger: ErrorLogger[E]): UIO[Unit] =
        Logger.logCause(causeLevel.some, message, cause, context*)

    }

    def apply[E](cause: zio.Cause[E], context: => (String, Any)*)(implicit trace: zio.Trace, errorLogger: ErrorLogger[E]): UIO[Unit] =
      apply(None, "", cause, context*)

    def apply[E](message: => Any, cause: zio.Cause[E], context: => (String, Any)*)(implicit trace: zio.Trace, errorLogger: ErrorLogger[E]): UIO[Unit] =
      apply(None, message, cause, context*)

    def apply(causeLevel: Logger.LogLevel): LogCauseAtLevel = new LogCauseAtLevel(causeLevel)

    object never extends LogCauseAtLevel(Logger.LogLevel.Never)
    object trace extends LogCauseAtLevel(Logger.LogLevel.Trace)
    object debug extends LogCauseAtLevel(Logger.LogLevel.Debug)
    object detailed extends LogCauseAtLevel(Logger.LogLevel.Detailed)
    object info extends LogCauseAtLevel(Logger.LogLevel.Info)
    object important extends LogCauseAtLevel(Logger.LogLevel.Important)
    object warning extends LogCauseAtLevel(Logger.LogLevel.Warning)
    object error extends LogCauseAtLevel(Logger.LogLevel.Error)
    object fatal extends LogCauseAtLevel(Logger.LogLevel.Fatal)
    object always extends LogCauseAtLevel(Logger.LogLevel.Always)

  }

  object logCauseStack {

    def apply[E](causeLevel: Option[Logger.LogLevel], message: => Any, cause: zio.Cause[E], context: => (String, Any)*)(implicit trace: zio.Trace, errorLogger: ErrorLogger[E]): UIO[Unit] =
      ZIO.stackTrace.flatMap { stackTrace =>
        val stackTrace2 = Logger.StackTrace.fromZio(stackTrace).some
        cause.failures.map(errorLogger.logLevel).maxByOption(_.logPriority).orElse(causeLevel) match
          case Some(level) =>
            Logger.execute(Logger.Event(level, String.valueOf(message), Logger.LogContext(context), Logger.Cause.fromZio(cause, errorLogger), Logger.Trace.fromZio(trace), stackTrace2))
          case None =>
            Logger.execute(Logger.Event(String.valueOf(message), Logger.LogContext(context), Logger.Cause.fromZio(cause, errorLogger), Logger.Trace.fromZio(trace), stackTrace2))
      }

    sealed class LogCauseAtLevel(causeLevel: Logger.LogLevel) {

      def apply[E](cause: zio.Cause[E], context: => (String, Any)*)(implicit trace: zio.Trace, errorLogger: ErrorLogger[E]): UIO[Unit] =
        Logger.logCauseStack(causeLevel.some, "", cause, context*)

      def apply[E](message: => Any, cause: zio.Cause[E], context: => (String, Any)*)(implicit trace: zio.Trace, errorLogger: ErrorLogger[E]): UIO[Unit] =
        Logger.logCauseStack(causeLevel.some, message, cause, context*)

    }

    def apply[E](cause: zio.Cause[E], context: => (String, Any)*)(implicit trace: zio.Trace, errorLogger: ErrorLogger[E]): UIO[Unit] =
      apply(None, "", cause, context*)

    def apply[E](message: => Any, cause: zio.Cause[E], context: => (String, Any)*)(implicit trace: zio.Trace, errorLogger: ErrorLogger[E]): UIO[Unit] =
      apply(None, message, cause, context*)

    def apply(causeLevel: Logger.LogLevel): LogCauseAtLevel = new LogCauseAtLevel(causeLevel)

    object never extends LogCauseAtLevel(Logger.LogLevel.Never)
    object trace extends LogCauseAtLevel(Logger.LogLevel.Trace)
    object debug extends LogCauseAtLevel(Logger.LogLevel.Debug)
    object detailed extends LogCauseAtLevel(Logger.LogLevel.Detailed)
    object info extends LogCauseAtLevel(Logger.LogLevel.Info)
    object important extends LogCauseAtLevel(Logger.LogLevel.Important)
    object warning extends LogCauseAtLevel(Logger.LogLevel.Warning)
    object error extends LogCauseAtLevel(Logger.LogLevel.Error)
    object fatal extends LogCauseAtLevel(Logger.LogLevel.Fatal)
    object always extends LogCauseAtLevel(Logger.LogLevel.Always)

  }

  // --- Other ---

  def getContext: UIO[Chunk[(String, String)]] =
    Logger.loggerRef.get.map(_.context)

  // =====| Types |=====

  type LogContext = Chunk[(String, String)]
  object LogContext {
    inline def apply(seq: Seq[(String, Any)]): LogContext = Chunk.fromIterable(seq).map { (k, v) => (k, String.valueOf(v)) }
  }

  sealed trait FiberId derives JsonCodec {

    final def toZio: zio.FiberId = this match
      case FiberId.None                                   => zio.FiberId.None
      case FiberId.Runtime(id, startTimeMillis, location) => zio.FiberId.Runtime(id, startTimeMillis, location.toZio)
      case FiberId.Composite(left, right)                 => zio.FiberId.Composite(left.toZio, right.toZio)

    final def show: String =
      this.toZio.threadName

  }
  object FiberId {

    case object None extends FiberId
    final case class Runtime(id: Int, startTimeMillis: Long, location: Trace) extends FiberId
    final case class Composite(left: FiberId, right: FiberId) extends FiberId

    def fromZio(fiberId: zio.FiberId): FiberId = fiberId match
      case zio.FiberId.None                                   => FiberId.None
      case zio.FiberId.Runtime(id, startTimeMillis, location) => FiberId.Runtime(id, startTimeMillis, Logger.Trace.fromZio(location))
      case zio.FiberId.Composite(left, right)                 => FiberId.Composite(FiberId.fromZio(left), FiberId.fromZio(right))

  }

  final case class Event(
      level: Option[Logger.LogLevel],
      _message: () => String,
      _context: () => Logger.LogContext,
      cause: Cause,
      trace: Trace,
      stackTrace: Option[StackTrace],
  ) {
    lazy val message: String = _message()
    lazy val context: Logger.LogContext = _context()
  }
  object Event {

    def apply(
        level: Logger.LogLevel,
        message: => String,
        context: => Logger.LogContext,
        cause: Cause,
        trace: Trace,
        stackTrace: Option[StackTrace],
    ): Event =
      new Event(
        level = level.some,
        _message = () => message,
        _context = () => context,
        cause = cause,
        trace = trace,
        stackTrace = stackTrace,
      )

    def apply(
        message: => String,
        context: => Logger.LogContext,
        cause: Cause,
        trace: Trace,
        stackTrace: Option[StackTrace],
    ): Event =
      new Event(
        level = None,
        _message = () => message,
        _context = () => context,
        cause = cause,
        trace = trace,
        stackTrace = stackTrace,
      )

  }

  sealed trait Cause derives JsonCodec {

    final def toZio: zio.Cause[Json] = this match
      case Cause.Empty                           => zio.Cause.Empty
      case Cause.Fail(value, Some(trace))        => zio.Cause.Fail(value, trace.toZio)
      case Cause.Die(value, Some(trace))         => zio.Cause.Die(value, trace.toZio)
      case Cause.Interrupt(fiberId, Some(trace)) => zio.Cause.Interrupt(fiberId.toZio, trace.toZio)
      case Cause.Then(left, right)               => zio.Cause.Then(left.toZio, right.toZio)
      case Cause.Both(left, right)               => zio.Cause.Both(left.toZio, right.toZio)
      case Cause.Fail(value, None)               => zio.Cause.stackless(zio.Cause.Fail(value, zio.StackTrace.none))
      case Cause.Die(value, None)                => zio.Cause.stackless(zio.Cause.Die(value, zio.StackTrace.none))
      case Cause.Interrupt(fiberId, None)        => zio.Cause.stackless(zio.Cause.Interrupt(fiberId.toZio, zio.StackTrace.none))

    final def toBases: List[Cause.Base] = this match
      case Cause.Empty             => Nil
      case base: Cause.Base        => base :: Nil
      case Cause.Then(left, right) => left.toBases ::: right.toBases
      case Cause.Both(left, right) => left.toBases ::: right.toBases

  }
  object Cause {

    sealed trait Base extends Cause

    case object Empty extends Cause
    final case class Fail(value: Json, trace: Option[StackTrace]) extends Cause.Base
    final case class Die(value: EncodedThrowable, trace: Option[StackTrace]) extends Cause.Base
    final case class Interrupt(fiberId: FiberId, trace: Option[StackTrace]) extends Cause.Base
    final case class Then(left: Cause, right: Cause) extends Cause
    final case class Both(left: Cause, right: Cause) extends Cause

    private def fromZio(cause: zio.Cause[Json], stack: Boolean): Cause = cause match
      case zio.Cause.Empty                       => Cause.Empty
      case zio.Cause.Fail(value, trace)          => Cause.Fail(value, Option.when(stack)(StackTrace.fromZio(trace)))
      case zio.Cause.Die(value, trace)           => Cause.Die(EncodedThrowable.fromThrowable(value), Option.when(stack)(StackTrace.fromZio(trace)))
      case zio.Cause.Interrupt(fiberId, trace)   => Cause.Interrupt(FiberId.fromZio(fiberId), Option.when(stack)(StackTrace.fromZio(trace)))
      case zio.Cause.Stackless(cause, stackless) => Cause.fromZio(cause, !stackless)
      case zio.Cause.Then(left, right)           => Cause.Then(Cause.fromZio(left, stack), Cause.fromZio(right, stack))
      case zio.Cause.Both(left, right)           => Cause.Both(Cause.fromZio(left, stack), Cause.fromZio(right, stack))

    def fromZio(cause: zio.Cause[Json]): Cause = Cause.fromZio(cause, true)
    def fromZio[E](cause: zio.Cause[E], errorLogger: ErrorLogger[E]): Cause = Cause.fromZio(cause.map(errorLogger.show), true)

  }

  final case class Trace(
      location: String,
      file: String,
      line: Int,
  ) derives JsonCodec {

    def show: String = s"$location($file:$line)"

    def toZio: zio.Trace = zio.internal.stacktracer.Tracer.instance(location, file, line)

  }
  object Trace {

    def fromZio(trace: zio.Trace): Trace = trace match
      case zio.internal.stacktracer.Tracer.instance(location, file, line) => Trace(location, file, line)
      case _                                                              => Trace(trace.toString, "unknown", 0)

    @nowarn
    def unapply(trace: String): Option[Trace] = trace match
      case zio.internal.stacktracer.Tracer.instance(location, file, line) => Trace(location, file, line).some
      case _                                                              => None

  }

  /**
    * Note: [[stackTrace]] will be [[FiberId.None]] if this [[StackTrace]] has been encoded/decoded.
    */
  final case class StackTrace private (
      fiberId: FiberId,
      stackTrace: Chunk[Trace],
  ) derives JsonCodec {

    def toZio: zio.StackTrace =
      zio.StackTrace(fiberId.toZio, stackTrace.map(_.toZio))

  }
  object StackTrace {

    def fromZio(stackTrace: zio.StackTrace): StackTrace =
      StackTrace(FiberId.fromZio(stackTrace.fiberId), stackTrace.stackTrace.map(Trace.fromZio))

  }

  final case class ExecutedEvent(
      logLevel: Option[LogLevel],
      message: String,
      context: Logger.LogContext,
      cause: Cause,
      trace: Trace,
      stackTrace: Option[StackTrace],
      timestamp: Instant,
  ) derives JsonCodec {

    // TODO (KR) : Option to show `dateTime` in log message
    def formatted(colorMode: ColorMode, logTimestamp: Boolean, logTrace: Boolean, logStack: Boolean): String = {

      // TODO (KR) : remove
      // =====|  |=====

      val (tmpMessage, tmpCause) = (message, cause) match {
        case ("", Cause.Fail(Json.Str(value), None)) => (value, Cause.Empty)
        case ("", Cause.Fail(value, None))           => (value.toJsonPretty, Cause.Empty)
        case _                                       => (message, cause)
      }

      // --- Base Message Part ---

      val msg: String =
        if (tmpMessage.contains('\n')) tmpMessage.replaceAll("\n", LogLevel.newLineIndent)
        else tmpMessage

      val fullContext: Logger.LogContext =
        Chunk(
          context,
          if (logTimestamp) Chunk("timestamp" -> timestamp.toString) // TODO (KR) : format?
          else Chunk.empty,
          if (logTrace) Chunk("trace" -> trace.show)
          else Chunk.empty,
        ).flatten
      def contextMsg =
        fullContext.map { (k, v) => s"${colorMode.fgColorize(Color.Named.Cyan, k)}=${colorMode.fgColorize(Color.Named.Magenta, v)}" }.mkString(" ; ")

      val baseMessagePart: String =
        if (fullContext.isEmpty) msg
        else if (msg.isEmpty) contextMsg
        else s"$contextMsg${LogLevel.newLineIndent}$msg"

      // --- Cause ---

      extension (self: Json)
        def showJson: IndentedString =
          (self match {
            case Json.Str(self) => self
            case _              => self.toJsonPretty
          }).split('\n').toList

      extension (self: Option[Logger.StackTrace])
        def showStackTrace: IndentedString =
          self.map { stackTrace =>
            val traces: Seq[Logger.Trace] = stackTrace.stackTrace
            IndentedString.section("StackTrace:")(
              s"fiber-id=${stackTrace.fiberId.show}",
              IndentedString.section("trace")(traces.map(_.show)),
            )
          }

      extension (self: Logger.Cause.Base)
        def showCause: IndentedString =
          self match {
            case Cause.Fail(value, trace) =>
              IndentedString.inline(
                IndentedString.section("Fail:")(value.showJson),
                trace.showStackTrace,
              )
            case Cause.Die(value, trace) =>
              IndentedString.inline(
                IndentedString.section("Die:")(value.safeToJsonAST.showJson),
                trace.showStackTrace,
              )
            case Cause.Interrupt(fiberId, trace) =>
              IndentedString.inline(
                IndentedString.section("Interrupt:")(s"fiber-id=${fiberId.show}"),
                trace.showStackTrace,
              )
          }

      val causeBases: List[(Logger.Cause.Base, Int)] =
        tmpCause.toBases.zipWithIndex

      def causeLabel(i: Int): String =
        s"cause-$i"

      val causeWidth: Int = causeLabel(causeBases.size - 1).length max (Logger.LogLevel.maxDisplayNameLength + 2)
      val emptyCauseString: String = s"\n${" " * causeWidth}: "

      val causeString: String =
        causeBases.map { case (cause, idx) =>
          s"\n${causeLabel(idx).alignLeft(causeWidth)}: ${cause.showCause.toString.replaceAll("\n", emptyCauseString)}"
        }.mkString

      // --- Stack Trace ---

      val stackString: String =
        (stackTrace, logStack) match {
          case (Some(stackTrace), true) =>
            val idtStr =
              IndentedString.inline(
                s"fiber-id=${stackTrace.fiberId.show}",
                (stackTrace.stackTrace: Seq[Logger.Trace]).map(t => s"- ${t.show}"),
              )
            s"\n${"stack".alignLeft(causeWidth)}: ${idtStr.toString.replaceAll("\n", emptyCauseString)}"
          case _ => ""
        }

      // --- Join All ---

      s"[${logLevel.fold(LogLevel.emptyDisplayName)(_.colorizedDisplayName(colorMode))}]: $baseMessagePart$causeString$stackString"
    }

  }

  trait Target {
    def log(event: ExecutedEvent): UIO[Unit]
  }
  object Target {

    def fromPrintStream(printStream: PrintStream, eventToString: ExecutedEvent => String): Target =
      event => ZIO.attempt { printStream.println(eventToString(event)) }.orDie

  }

  final case class Source(
      name: String,
      target: URIO[Scope, Target],
      minLogTolerance: Option[LogLevel],
      args: Chunk[(String, String)],
  ) {

    override def toString: String = {
      val allArgs: Chunk[(String, String)] = ("level", minLogTolerance.fold("default")(_.rawDisplayName)) +: args
      s"Source[$name${allArgs.map { case (k, v) => s", $k=$v" }.mkString}]"
    }

  }
  object Source {

    object names {

      val stdOut = "std-out"
      val stdOutJson = "std-out-json"
      val stringBuilder = "string-builder"

      val allStdOut: Set[String] = Set(stdOut, stdOutJson)

    }

    def const(
        name: String,
        target: Target,
        minLogTolerance: Option[LogLevel],
        args: (String, String)*,
    ): Source =
      Source(
        name,
        ZIO.succeed(target),
        minLogTolerance,
        Chunk.fromIterable(args),
      )

    def stdOut(
        minLogTolerance: Option[LogLevel],
        colorMode: ColorMode,
        logTimestamp: Boolean,
        logTrace: Boolean,
        logStack: Boolean,
    ): Source =
      Source.const(
        names.stdOut,
        Target.fromPrintStream(scala.Console.out, _.formatted(colorMode, logTimestamp, logTrace, logStack)),
        minLogTolerance,
      )

    def stdOutJson(
        minLogTolerance: Option[LogLevel],
    ): Source =
      Source.const(
        names.stdOutJson,
        Target.fromPrintStream(scala.Console.out, _.toJson),
        minLogTolerance,
      )

    def stringBuilder(
        sb: mutable.StringBuilder,
        minLogTolerance: Option[LogLevel],
        colorMode: ColorMode,
        logTimestamp: Boolean,
        logTrace: Boolean,
        logStack: Boolean,
    ): Source =
      Source.const(
        names.stringBuilder,
        event => ZIO.succeed { sb.append(event.formatted(colorMode, logTimestamp, logTrace, logStack)); sb.append('\n') },
        minLogTolerance,
      )

  }

  private def zioLevelBetween(label: String, a: zio.LogLevel, b: zio.LogLevel): zio.LogLevel =
    zio.LogLevel((a.ordinal + b.ordinal) / 2, label, (a.syslog + b.syslog) / 2)

  enum LogLevel(
      final val rawDisplayName: String,
      final val tolerancePriority: Int,
      final val logPriority: Int,
      final val extendedColor: Color,
      final val simpleColor: Color.Simple,
      final val zioLogLevel: zio.LogLevel,
  ) extends Enum[LogLevel] {

    case Never
        extends LogLevel(
          rawDisplayName = "NEVER",
          tolerancePriority = 10,
          logPriority = 0,
          extendedColor = Color.Default,
          simpleColor = Color.Default,
          zioLogLevel = zio.LogLevel(Int.MinValue, "NEVER", 7),
        )

    case Trace
        extends LogLevel(
          rawDisplayName = "TRACE",
          tolerancePriority = 1,
          logPriority = 1,
          extendedColor = Color.RGB.hex("#30f2c2"),
          simpleColor = Color.Named.Cyan,
          zioLogLevel = zio.LogLevel.Trace,
        )

    case Debug
        extends LogLevel(
          rawDisplayName = "DEBUG",
          tolerancePriority = 2,
          logPriority = 2,
          extendedColor = Color.RGB.hex("#0277bd"),
          simpleColor = Color.Named.Cyan,
          zioLogLevel = zio.LogLevel.Debug,
        )

    case Detailed
        extends LogLevel(
          rawDisplayName = "DETLD",
          tolerancePriority = 3,
          logPriority = 3,
          extendedColor = Color.RGB.hex("#66bb6a"),
          simpleColor = Color.Named.Blue,
          zioLogLevel = zioLevelBetween("DETAILED", zio.LogLevel.Debug, zio.LogLevel.Info),
        )

    case Info
        extends LogLevel(
          rawDisplayName = "INFO",
          tolerancePriority = 4,
          logPriority = 4,
          extendedColor = Color.RGB.hex("#1b5e20"),
          simpleColor = Color.Named.Green,
          zioLogLevel = zio.LogLevel.Info,
        )

    case Important
        extends LogLevel(
          rawDisplayName = "IMPRT",
          tolerancePriority = 5,
          logPriority = 5,
          extendedColor = Color.RGB.hex("#880e4f"),
          simpleColor = Color.Named.Yellow,
          zioLogLevel = zioLevelBetween("IMPORTANT", zio.LogLevel.Info, zio.LogLevel.Warning),
        )

    case Warning
        extends LogLevel(
          rawDisplayName = "WARN",
          tolerancePriority = 6,
          logPriority = 6,
          extendedColor = Color.RGB.hex("#ffff00"),
          simpleColor = Color.Named.Yellow,
          zioLogLevel = zio.LogLevel.Warning,
        )

    case Error
        extends LogLevel(
          rawDisplayName = "ERROR",
          tolerancePriority = 7,
          logPriority = 7,
          extendedColor = Color.RGB.hex("#ff3d00"),
          simpleColor = Color.Named.Red,
          zioLogLevel = zio.LogLevel.Error,
        )

    case Fatal
        extends LogLevel(
          rawDisplayName = "FATAL",
          tolerancePriority = 8,
          logPriority = 8,
          extendedColor = Color.RGB.hex("#d50000"),
          simpleColor = Color.Named.Red,
          zioLogLevel = zio.LogLevel.Fatal,
        )

    case Always
        extends LogLevel(
          rawDisplayName = "ALWYS",
          tolerancePriority = 9,
          logPriority = 9,
          extendedColor = Color.Default,
          simpleColor = Color.Default,
          zioLogLevel = zio.LogLevel(Int.MaxValue, "ALWAYS", 0),
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
  object LogLevel extends Enum.Companion[LogLevel] {

    override protected val defaultToString: LogLevel => NonEmptyList[String] = l => NonEmptyList.of(l.rawDisplayName, l.name)

    lazy val maxDisplayNameLength: Int =
      enumValues.map(_.rawDisplayName.length).max

    lazy val emptyDisplayName: String =
      " " * maxDisplayNameLength

    lazy val newLineIndent: String =
      s"\n $emptyDisplayName : "

    implicit val jsonCodec: JsonCodec[LogLevel] = JsonCodec.fromHarnessStringEncoderAndDecoder

  }

}
