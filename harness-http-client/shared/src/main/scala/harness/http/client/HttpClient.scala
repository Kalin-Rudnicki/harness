package harness.http.client

import harness.zio.*
import zio.*

trait HttpClient[-RequestBody, ResponseBody] { self =>

  protected def sendImpl(request: HttpRequest[RequestBody], cors: Boolean): HRIO[Logger & Scope, HttpResponse.Result[ResponseBody]]

  final def send(request: HttpRequest[RequestBody], cors: Boolean = false): HRIO[Logger & Telemetry & Scope, HttpResponse[ResponseBody]] =
    Logger.log.debug(s"Sending HTTP request to: ${request.url}") *>
      self.sendImpl(request, cors).flatMap(HttpResponse.fromResult).telemetrize("HTTP Client Send", "url" -> request.url)

  final def sendAndUse[R, T](request: HttpRequest[RequestBody], cors: Boolean = false)(use: HttpResponse[ResponseBody] => HRIO[R & Scope, T]): HRIO[R & Logger & Telemetry, T] =
    ZIO.scoped { self.send(request, cors).flatMap(use) }

}
object HttpClient extends HttpClientPlatformSpecific with HttpClientPlatformSpecificImpl {

  def send(request: HttpRequest[RequestT], cors: Boolean = false): HRIO[HttpClient.ClientT & Logger & Telemetry & Scope, HttpResponse[ResponseT]] =
    ZIO.serviceWithZIO[HttpClient[RequestT, ResponseT]](_.send(request, cors))

  def sendAndUse[R, T](request: HttpRequest[RequestT], cors: Boolean = false)(use: HttpResponse[ResponseT] => HRIO[R & Scope, T]): HRIO[R & HttpClient.ClientT & Logger & Telemetry, T] =
    ZIO.serviceWithZIO[HttpClient[RequestT, ResponseT]](_.sendAndUse(request, cors)(use))

}
