package harness.zio

import harness.core.*
import java.time.OffsetDateTime
import zio.*
import zio.json.*

trait Telemetry { self =>

  def trace(event: Telemetry.Trace): URIO[Logger, Boolean]

  final def withMinLogTolerance(min: Logger.LogLevel): Telemetry =
    new Telemetry {
      override def trace(event: Telemetry.Trace): URIO[Logger, Boolean] =
        ZIO.when(event.logLevel.logPriority >= min.tolerancePriority)(self.trace(event)).map(_.getOrElse(false))
    }

  final def ||(other: Telemetry): Telemetry =
    new Telemetry {
      override def trace(event: Telemetry.Trace): URIO[Logger, Boolean] =
        self.trace(event) || other.trace(event)
    }

  final def &&(other: Telemetry): Telemetry =
    new Telemetry {
      override def trace(event: Telemetry.Trace): URIO[Logger, Boolean] =
        (self.trace(event) <&> other.trace(event)).map { _ || _ }
    }

}
object Telemetry {

  val configLayer: HRLayer[TelemetryConfig, Telemetry] = ZLayer.service[TelemetryConfig].project(_.telemetry)

  // =====| Api |=====

  def trace[R, E, A](
      effect: ZIO[R, E, A],
      label: String,
      logLevel: Logger.LogLevel,
      telemetryContext: Map[String, String],
  ): ZIO[Telemetry & Logger & R, E, A] =
    Clock.currentDateTime.flatMap { startDateTime =>
      effect.foldCauseZIO(
        { cause =>
          Clock.currentDateTime.flatMap { endDateTime =>
            Telemetry.trace(label, logLevel, startDateTime, endDateTime, false, telemetryContext) *> ZIO.failCause(cause)
          }
        },
        { a =>
          Clock.currentDateTime.flatMap { endDateTime =>
            Telemetry.trace(label, logLevel, startDateTime, endDateTime, true, telemetryContext) *> ZIO.succeed(a)
          }
        },
      )
    }

  def trace(
      label: String,
      logLevel: Logger.LogLevel,
      startDateTime: OffsetDateTime,
      endDateTime: OffsetDateTime,
      success: Boolean,
      telemetryContext: Map[String, String],
  ): URIO[Telemetry & Logger, Unit] =
    for {
      telemetry <- ZIO.service[Telemetry]
      logContext <- Logger.getContext
      _ <- telemetry.trace(Telemetry.Trace(logLevel, label, startDateTime, endDateTime, success, telemetryContext, logContext))
    } yield ()

  // =====| Types |=====

  final case class Trace(
      logLevel: Logger.LogLevel,
      label: String,
      startDateTime: OffsetDateTime,
      endDateTime: OffsetDateTime,
      success: Boolean,
      telemetryContext: Map[String, String],
      logContext: Map[String, String],
  ) { self =>

    def duration: Duration = java.time.Duration.between(startDateTime, endDateTime)

    def toLoggerEvent: Logger.Event =
      Logger.Event.AtLogLevel(
        self.logLevel,
        { () =>
          Logger.Event.Output(
            self.telemetryContext + ("success" -> self.success.toString),
            s"TELEMETRY - (${self.label}) [${self.duration.prettyPrint}]${if (self.success) "" else " *** FAIL ***"}",
          )
        },
      )

  }
  object Trace {
    implicit val jsonCodec: JsonCodec[Trace] = DeriveJsonCodec.gen
  }

  // =====| Layers |=====

  val none: Telemetry =
    new Telemetry {
      override def trace(event: Telemetry.Trace): URIO[Logger, Boolean] = ZIO.succeed(false)
    }

  val log: Telemetry =
    new Telemetry {
      override def trace(event: Telemetry.Trace): URIO[Logger, Boolean] =
        Logger.execute { event.toLoggerEvent }.as(true)
    }

}
