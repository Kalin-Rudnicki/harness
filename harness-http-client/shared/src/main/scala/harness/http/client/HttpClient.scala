package harness.http.client

import harness.zio.*
import zio.*

trait HttpClient[-RequestBody, ResponseBody] { self =>

  protected def sendImpl(request: HttpRequest[RequestBody]): HRIO[Logger & Scope, HttpResponse.Result[ResponseBody]]

  final def send(request: HttpRequest[RequestBody]): HRIO[Logger & Telemetry & Scope, HttpResponse[ResponseBody]] =
    Logger.log.debug(s"Sending HTTP request to: ${request.url}") *>
      self.sendImpl(request).flatMap(HttpResponse.fromResult).trace("HTTP Client Send", "url" -> request.url)

  final def sendAndUse[R, T](request: HttpRequest[RequestBody])(use: HttpResponse[ResponseBody] => HRIO[R & Scope, T]): HRIO[R & Logger & Telemetry, T] =
    ZIO.scoped { self.send(request).flatMap(use) }

}
object HttpClient extends HttpClientPlatformSpecific with HttpClientPlatformSpecificImpl {

  def send(request: HttpRequest[RequestT]): HRIO[HttpClient.ClientT & Logger & Telemetry & Scope, HttpResponse[ResponseT]] =
    ZIO.serviceWithZIO[HttpClient[RequestT, ResponseT]](_.send(request))

  def sendAndUse[R, T](request: HttpRequest[RequestT])(use: HttpResponse[ResponseT] => HRIO[R & Scope, T]): HRIO[R & HttpClient.ClientT & Logger & Telemetry, T] =
    ZIO.serviceWithZIO[HttpClient[RequestT, ResponseT]](_.sendAndUse(request)(use))

}
