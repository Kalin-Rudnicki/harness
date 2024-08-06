package harness.zio

import java.time.Instant
import zio.*
import zio.json.*

trait Telemetry {

  def telemetrize(event: Telemetry.Trace): UIO[Boolean]

  final def telemetrize[R, E, A](
      effect: ZIO[R, E, A],
      label: String,
      logLevel: Logger.LogLevel,
      telemetryContext: Logger.LogContext,
      trace: Logger.Trace,
  ): ZIO[R, E, A] =
    (for {
      start <- Clock.instant
      logContext <- Logger.getContext
      exit <- effect.interruptible.exit
      end <- Clock.instant
      _ <- this.telemetrize(Telemetry.Trace(logLevel, label, start, end, exit.isSuccess, telemetryContext, logContext, trace))
      res <- exit
    } yield res).uninterruptible

  final def withMinLogTolerance(min: Logger.LogLevel): Telemetry =
    event =>
      if (event.logLevel.logPriority >= min.tolerancePriority) this.telemetrize(event)
      else ZIO.succeed(false)

  final def withMinLogTolerance(min: Option[Logger.LogLevel]): Telemetry = min match
    case Some(min) => withMinLogTolerance(min)
    case None      => this

  final def ||(that: Telemetry): Telemetry =
    event => this.telemetrize(event) || that.telemetrize(event)

  final def &&(that: Telemetry): Telemetry =
    event => (this.telemetrize(event) <&> that.telemetrize(event)).map { _ || _ }

}
object Telemetry {

  // =====| API |=====

  // --- Modify ---

  def withTelemetry(f: Telemetry => Telemetry): FiberRefModification =
    Telemetry.telemetryRef.modification.update(f)
  def withTelemetry(telemetry: Telemetry): FiberRefModification =
    Telemetry.telemetryRef.modification.set(telemetry)

  def withMinLogTolerance(min: Logger.LogLevel): FiberRefModification =
    Telemetry.telemetryRef.modification.update(_.withMinLogTolerance(min))

  // --- Execute ---

  def telemetrize(
      label: String,
      logLevel: Logger.LogLevel,
      telemetryContext: Logger.LogContext,
  ): ZIOAspectPoly =
    new ZIOAspectPoly.Impl {
      override def apply[R, E, A](effect: ZIO[R, E, A])(implicit trace: zio.Trace): ZIO[R, E, A] =
        Telemetry.telemetryRef.getWith(_.telemetrize(effect, label, logLevel, telemetryContext, Logger.Trace.fromZio(trace)))
    }

  // =====| Types |=====

  final class StartMarker(start: Instant) {

    def markEnd(label: String, logLevel: Logger.LogLevel, success: Boolean, telemetryContext: (String, Any)*)(implicit trace: zio.Trace): UIO[Unit] =
      for {
        end <- Clock.instant
        telemetry <- Telemetry.telemetryRef.get
        logContext <- Logger.getContext
        _ <- telemetry.telemetrize(Telemetry.Trace(logLevel, label, start, end, success, Logger.LogContext(telemetryContext), logContext, Logger.Trace.fromZio(trace)))
      } yield ()

  }
  object StartMarker {
    def make: UIO[StartMarker] = Clock.instant.map(new StartMarker(_))
  }

  final case class Trace(
      logLevel: Logger.LogLevel,
      label: String,
      startInstant: Instant,
      endInstant: Instant,
      success: Boolean,
      telemetryContext: Logger.LogContext,
      logContext: Logger.LogContext,
      trace: Logger.Trace,
  ) { self =>

    def duration: Duration = Duration.fromInterval(startInstant, endInstant)

    def toLoggerEvent: Logger.Event =
      Logger.Event(
        self.logLevel,
        "TELEMETRY",
        self.telemetryContext ++ Chunk("label" -> self.label, "duration" -> self.duration.render, "success" -> self.success.toString),
        Logger.Cause.Empty,
        trace,
        None,
      )

  }
  object Trace {
    implicit val jsonCodec: JsonCodec[Trace] = DeriveJsonCodec.gen
  }
  // =====|  |=====

  val none: Telemetry =
    _ => ZIO.succeed(false)

  val log: Telemetry =
    event => Logger.execute { event.toLoggerEvent }.as(true)

  private[zio] val telemetryRef: FiberRef[Telemetry] =
    Unsafe.unsafely {
      FiberRef.unsafe.make[Telemetry](
        Telemetry.log,
        identity,
        (_, child) => child,
      )
    }

}
