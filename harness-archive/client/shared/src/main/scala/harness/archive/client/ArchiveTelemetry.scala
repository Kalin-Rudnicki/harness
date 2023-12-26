package harness.archive.client

import cats.syntax.either.*
import harness.archive.model as D
import harness.core.*
import harness.http.client.*
import harness.zio.*
import zio.*
import zio.json.*

final class ArchiveTelemetry(
    appName: String,
    baseUrl: String,
    httpClient: HttpClient.ClientT,
) extends Telemetry {

  private val env: ZEnvironment[HttpClient.ClientT & Logger & Telemetry] = ZEnvironment(httpClient, Logger.none, Telemetry.none)

  private val uploadUrl: String = s"$baseUrl/api/telemetry/upload"

  // TODO (KR) : be smarter about batching multiple requests
  override def telemetrize(event: Telemetry.Trace): URIO[Logger, Boolean] =
    ZIO
      .suspendSucceed {
        val payload =
          D.telemetry.Upload(
            appName = appName,
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

  val keyedConfigDecoder: HConfig.KeyedConfigDecoder[Telemetry] =
    HConfig.KeyedConfigDecoder.make[ArchiveConfig, Telemetry]("harness-archive") { config =>
      new ArchiveTelemetry(config.appName, config.baseUrl, HttpClient.defaultClient).withMinLogTolerance(config.logTolerance).asRight
    }

}
