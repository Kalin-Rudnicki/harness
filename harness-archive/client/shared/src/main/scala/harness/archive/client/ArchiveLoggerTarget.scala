package harness.archive.client

import cats.syntax.option.*
import harness.archive.model as D
import harness.http.client.*
import harness.zio.*
import zio.*
import zio.json.*

final class ArchiveLoggerTarget(sender: QueuedSender[Any, Logger.ExecutedEvent]) extends Logger.Target {

  override def log(event: Logger.ExecutedEvent): UIO[Unit] =
    sender.push(event)

}
object ArchiveLoggerTarget {

  private def makeConfigSource(cfg: ArchiveConfig): LoggerConfig.Src =
    LoggerConfig.Src {
      for {
        httpClient <- ZIO.succeed(HttpClient.defaultClient)
        env: ZEnvironment[HttpClient.ClientT & Logger & Telemetry] = ZEnvironment(httpClient, Logger.none, Telemetry.none)
        uploadUrl: String = s"${cfg.baseUrl}/api/log/upload"
        sender <- QueuedSender.make[Any, Logger.ExecutedEvent]("ArchiveLoggerTarget", cfg.queueChunkSize, cfg.queueDumpEvery, true) { events =>
          ZIO
            .suspendSucceed {
              val payload =
                D.log.Upload(
                  cfg.appId,
                  events.map { event =>
                    D.log.Upload.Log(
                      logLevel = event.logLevel,
                      message = event.message,
                      context = event.context,
                      dateTime = event.dateTime,
                    )
                  },
                )

              HttpRequest
                .post(uploadUrl)
                .withHeader("ARCHIVE-APP-TOKEN", cfg.appToken)
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
      } yield Logger.Source.const(new ArchiveLoggerTarget(sender), cfg.logTolerance.some)
    }

  val keyedConfigDecoder: HConfig.KeyedConfigDecoder[LoggerConfig.Src] =
    HConfig.KeyedConfigDecoder.make[ArchiveConfig, LoggerConfig.Src]("harness-archive") { ArchiveLoggerTarget.makeConfigSource }

}
