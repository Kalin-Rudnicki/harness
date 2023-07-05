package harness.archive.client

import harness.archive.model as D
import harness.core.*
import harness.http.client.*
import harness.zio.*
import zio.*
import zio.json.*

final class ArchiveTelemetry(spec: ArchiveSpec) extends Telemetry {

  private val env: ZEnvironment[HttpClient.ClientT & Logger & Telemetry] = ZEnvironment(spec.httpClient, Logger.none, Telemetry.none)

  private val uploadUrl: String = s"${spec.baseUrl}/api/telemetry/upload"

  override def trace(event: Telemetry.Trace): URIO[Logger, Boolean] =
    ZIO
      .suspendSucceed {
        val payload =
          D.telemetry.Upload(
            appName = spec.appName,
            logLevel = event.logLevel,
            label = event.label,
            startDateTime = event.startDateTime,
            endDateTime = event.endDateTime,
            success = event.success,
            telemetryContext = event.telemetryContext,
            logContext = event.logContext,
          )

        HttpRequest
          .post(uploadUrl)
          .withBodyJsonEncoded(payload :: Nil)
          .response
          .unit2xx
          .provideEnvironment(env)
          .foldZIO(
            error => Util.logToConsole(error.toNel.toList.map(_.fullInternalMessage).mkString("\n")) *> Util.logToConsole(payload.toJson),
            _ => ZIO.unit,
          )
      }
      .fork
      .as(true)

}
object ArchiveTelemetry {

  val layer: URLayer[ArchiveSpec, Telemetry] =
    ZLayer.fromZIO { ZIO.serviceWith[ArchiveSpec](new ArchiveTelemetry(_)) }

}
