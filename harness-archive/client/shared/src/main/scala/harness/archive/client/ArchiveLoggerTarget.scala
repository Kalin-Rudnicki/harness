package harness.archive.client

import harness.archive.model as D
import harness.core.*
import harness.http.client.*
import harness.zio.*
import zio.*
import zio.json.*

final class ArchiveLoggerTarget(spec: ArchiveSpec) extends Logger.Target {

  private val env: ZEnvironment[HttpClient.ClientT & Logger & Telemetry] = ZEnvironment(spec.httpClient, Logger.none, Telemetry.none)

  private val uploadUrl: String = s"${spec.baseUrl}/api/log/upload"

  // TODO (KR) : be smarter about batching multiple requests
  override def log(event: Logger.ExecutedEvent): UIO[Unit] =
    ZIO
      .suspendSucceed {
        val payload =
          D.log.Upload(
            appName = spec.appName,
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

  def loggerLayerWithSource(minLogTolerance: Option[Logger.LogLevel], colorMode: Option[ColorMode]): URLayer[ArchiveSpec & Logger, Logger] =
    for {
      spec <- ZLayer.service[ArchiveSpec]
      logger <- Logger.withSources(Logger.Source.const(new ArchiveLoggerTarget(spec.get), minLogTolerance, colorMode))
    } yield logger

}
