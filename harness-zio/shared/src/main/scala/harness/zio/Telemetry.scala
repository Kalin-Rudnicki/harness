package harness.zio

import harness.core.*
import java.time.OffsetDateTime
import zio.*

trait Telemetry { self =>

  val appName: String
  def trace(event: Telemetry.Trace): URIO[Logger, Boolean]

  final def withMinLogTolerance(min: Logger.LogLevel): Telemetry =
    new Telemetry {
      override val appName: String = self.appName
      override def trace(event: Telemetry.Trace): URIO[Logger, Boolean] =
        ZIO.when(event.logLevel.logPriority >= min.tolerancePriority)(self.trace(event)).map(_.getOrElse(false))
    }

  final def ||(other: Telemetry): Telemetry =
    new Telemetry {
      override val appName: String = List(self.appName, other.appName).filter(_.nonEmpty).mkString("/")
      override def trace(event: Telemetry.Trace): URIO[Logger, Boolean] =
        self.trace(event.copy(appName = self.appName)).flatMap {
          case true  => ZIO.succeed(true)
          case false => other.trace(event.copy(appName = other.appName))
        }
    }

  final def &&(other: Telemetry): Telemetry =
    new Telemetry {
      override val appName: String = List(self.appName, other.appName).filter(_.nonEmpty).mkString("/")
      override def trace(event: Telemetry.Trace): URIO[Logger, Boolean] =
        (self.trace(event.copy(appName = self.appName)) <&> other.trace(event.copy(appName = other.appName))).map { _ || _ }
    }

}
object Telemetry {

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
      _ <- telemetry.trace(Telemetry.Trace(telemetry.appName, logLevel, label, startDateTime, endDateTime, success, telemetryContext, logContext))
    } yield ()

  // =====| Types |=====

  final case class Trace(
      appName: String,
      logLevel: Logger.LogLevel,
      label: String,
      startDateTime: OffsetDateTime,
      endDateTime: OffsetDateTime,
      success: Boolean,
      telemetryContext: Map[String, String],
      logContext: Map[String, String],
  ) {
    def duration: Duration = java.time.Duration.between(startDateTime, endDateTime)
  }

  // =====| Layers |=====

  def none: Telemetry =
    new Telemetry {
      override val appName: String = ""
      override def trace(event: Telemetry.Trace): URIO[Logger, Boolean] = ZIO.succeed(false)
    }

  def log(_appName: String = ""): Telemetry =
    new Telemetry {
      override val appName: String = _appName
      override def trace(event: Telemetry.Trace): URIO[Logger, Boolean] =
        Logger
          .execute {
            Logger.Event.AtLogLevel(
              event.logLevel,
              { () =>
                Logger.Event.Output(
                  event.telemetryContext + ("success" -> event.success.toString),
                  s"TELEMETRY -${if (appName.isEmpty) "" else s" <$appName>"} (${event.label}) [${event.duration.prettyPrint}]${if (event.success) "" else " *** FAIL ***"}",
                )
              },
            )
          }
          .as(true)
    }

}
