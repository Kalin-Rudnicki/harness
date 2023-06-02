package harness.archive.client

import harness.archive.model as D
import harness.http.client.*
import harness.zio.*
import zio.*

final class ArchiveTarget(appName: String, baseUrl: String, httpClient: HttpClient.ClientT) extends Logger.Target {

  private val env: ZEnvironment[HttpClient.ClientT & Logger & Telemetry] = ZEnvironment(httpClient, Logger.none, Telemetry.none)
  private val stdOut: Logger.Target = Logger.Target.fromPrintStream("StdOut", scala.Console.out, _.formatted)

  private val uploadUrl: String = s"$baseUrl/api/log/upload"

  // TODO (KR) : be smarter about batching multiple requests
  override def log(event: Logger.ExecutedEvent): UIO[Unit] =
    ZIO
      .suspendSucceed {
        HttpRequest
          .post(uploadUrl)
          .withBodyJsonEncoded(
            D.log.Upload(
              appName = appName,
              logLevel = event.logLevel,
              message = event.message,
              context = event.context,
              dateTime = event.dateTime,
            ) :: Nil,
          )
          .response
          .unit2xx
          .provideEnvironment(env)
          .foldZIO(
            error => ZIO.succeed { println(error.toNel.toList.map(_.fullInternalMessage).mkString("\n")) } *> stdOut.log(event),
            _ => ZIO.unit,
          )
      }
      .fork
      .unit

}
