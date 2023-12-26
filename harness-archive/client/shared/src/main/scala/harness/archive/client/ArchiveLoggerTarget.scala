package harness.archive.client

import cats.syntax.either.*
import cats.syntax.option.*
import harness.archive.model as D
import harness.core.*
import harness.http.client.*
import harness.zio.*
import zio.*
import zio.json.*

final class ArchiveLoggerTarget(
    appName: String,
    baseUrl: String,
    httpClient: HttpClient.ClientT,
) extends Logger.Target {

  private val env: ZEnvironment[HttpClient.ClientT & Logger & Telemetry] = ZEnvironment(httpClient, Logger.none, Telemetry.none)

  private val uploadUrl: String = s"$baseUrl/api/log/upload"

  // TODO (KR) : be smarter about batching multiple requests
  override def log(event: Logger.ExecutedEvent): UIO[Unit] =
    ZIO
      .suspendSucceed {
        val payload =
          D.log.Upload(
            appName = appName,
            logLevel = event.logLevel,
            message = event.message,
            context = event.context,
            dateTime = event.dateTime,
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
      .unit

}
object ArchiveLoggerTarget {

  val keyedConfigDecoder: HConfig.KeyedConfigDecoder[Logger.Source] =
    HConfig.KeyedConfigDecoder.make[ArchiveConfig, Logger.Source]("harness-archive") { config =>
      Logger.Source.const(new ArchiveLoggerTarget(config.appName, config.baseUrl, HttpClient.defaultClient), config.logTolerance.some).asRight
    }

}
