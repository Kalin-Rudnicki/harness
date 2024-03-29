package harness.zio

import harness.core.*
import java.time.OffsetDateTime
import zio.*
import zio.json.*

trait Telemetry { self =>

  def telemetrize(event: Telemetry.Trace): URIO[Logger, Boolean]

  final def withMinLogTolerance(min: Logger.LogLevel): Telemetry =
    new Telemetry {
      override def telemetrize(event: Telemetry.Trace): URIO[Logger, Boolean] =
        ZIO.when(event.logLevel.logPriority >= min.tolerancePriority)(self.telemetrize(event)).map(_.getOrElse(false))
    }

  final def ||(other: Telemetry): Telemetry =
    new Telemetry {
      override def telemetrize(event: Telemetry.Trace): URIO[Logger, Boolean] =
        self.telemetrize(event) || other.telemetrize(event)
    }

  final def &&(other: Telemetry): Telemetry =
    new Telemetry {
      override def telemetrize(event: Telemetry.Trace): URIO[Logger, Boolean] =
        (self.telemetrize(event) <&> other.telemetrize(event)).map { _ || _ }
    }

}
object Telemetry {

  val configLayer: URLayer[TelemetryConfig & Scope, Telemetry] =
    ZLayer.fromZIO { ZIO.serviceWithZIO[TelemetryConfig](_.telemetry) }

  // =====| Api |=====

  def telemetrize[R, E, A](
      effect: ZIO[R, E, A],
      label: String,
      logLevel: Logger.LogLevel,
      telemetryContext: Map[String, String],
  ): ZIO[Telemetry & Logger & R, E, A] =
    Clock.currentDateTime.flatMap { startDateTime =>
      effect.foldCauseZIO(
        { cause =>
          Clock.currentDateTime.flatMap { endDateTime =>
            Telemetry.telemetrize(label, logLevel, startDateTime, endDateTime, false, telemetryContext) *> ZIO.failCause(cause)
          }
        },
        { a =>
          Clock.currentDateTime.flatMap { endDateTime =>
            Telemetry.telemetrize(label, logLevel, startDateTime, endDateTime, true, telemetryContext) *> ZIO.succeed(a)
          }
        },
      )
    }

  def telemetrize(
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
      _ <- telemetry.telemetrize(Telemetry.Trace(logLevel, label, startDateTime, endDateTime, success, telemetryContext, logContext))
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
      override def telemetrize(event: Telemetry.Trace): URIO[Logger, Boolean] = ZIO.succeed(false)
    }

  val log: Telemetry =
    new Telemetry {
      override def telemetrize(event: Telemetry.Trace): URIO[Logger, Boolean] =
        Logger.execute { event.toLoggerEvent }.as(true)
    }

}
