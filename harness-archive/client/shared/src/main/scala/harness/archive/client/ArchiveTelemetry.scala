package harness.archive.client

import harness.archive.model as D
import harness.http.client.*
import harness.zio.*
import zio.*
import zio.json.*

final class ArchiveTelemetry(sender: QueuedSender[Any, Telemetry.Trace]) extends Telemetry {

  override def telemetrize(event: Telemetry.Trace): URIO[Logger, Boolean] =
    sender.push(event).as(true)

}
object ArchiveTelemetry {

  private def makeConfigSource(cfg: ArchiveConfig): TelemetryConfig.Src =
    TelemetryConfig.Src {
      for {
        httpClient <- ZIO.succeed(HttpClient.defaultClient)
        env: ZEnvironment[HttpClient.ClientT & Logger & Telemetry] = ZEnvironment(httpClient, Logger.none, Telemetry.none)
        uploadUrl: String = s"${cfg.baseUrl}/api/telemetry/upload"
        sender <- QueuedSender.make[Any, Telemetry.Trace]("ArchiveTelemetry", cfg.queueChunkSize, cfg.queueDumpEvery, true) { events =>
          ZIO
            .suspendSucceed {
              val payload =
                events.map { event =>
                  D.telemetry.Upload(
                    appName = cfg.appName,
                    logLevel = event.logLevel,
                    label = event.label,
                    startDateTime = event.startDateTime,
                    endDateTime = event.endDateTime,
                    success = event.success,
                    telemetryContext = event.telemetryContext,
                    logContext = event.logContext,
                  )
                }

              HttpRequest
                .post(uploadUrl)
                .withBodyJsonEncoded(payload)
                .response
                .unit2xx
                .provideEnvironment(env)
                .foldZIO(
                  error => Util.logToConsole(error.toNel.toList.map(_.fullInternalMessage).mkString("\n")) *> Util.logToConsole(payload.toJson),
                  _ => ZIO.unit,
                )
            }
        }
      } yield new ArchiveTelemetry(sender).withMinLogTolerance(cfg.logTolerance)
    }

  val keyedConfigDecoder: HConfig.KeyedConfigDecoder[TelemetryConfig.Src] =
    HConfig.KeyedConfigDecoder.make[ArchiveConfig, TelemetryConfig.Src]("harness-archive") { ArchiveTelemetry.makeConfigSource }

}
