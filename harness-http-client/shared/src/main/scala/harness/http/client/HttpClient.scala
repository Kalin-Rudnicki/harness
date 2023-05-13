package harness.http.client

import harness.zio.*
import zio.*

trait HttpClient[-RequestBody, ResponseBody] { self =>

  protected def sendImpl(request: HttpRequest[RequestBody]): HRIO[Logger & Scope, HttpResponse.Result[ResponseBody]]

  final def send(request: HttpRequest[RequestBody]): HRIO[Logger & Scope, HttpResponse[ResponseBody]] =
    self.sendImpl(request).flatMap(HttpResponse.fromResult)

  final def sendAndUse[R, T](request: HttpRequest[RequestBody])(use: HttpResponse[ResponseBody] => HRIO[R & Scope, T]): HRIO[R & Logger, T] =
    ZIO.scoped { self.send(request).flatMap(use) }

}
object HttpClient extends HttpClientPlatformSpecific with HttpClientPlatformSpecificImpl {

  def send(request: HttpRequest[RequestT]): HRIO[HttpClient.ClientT & Logger & Scope, HttpResponse[ResponseT]] =
    ZIO.serviceWithZIO[HttpClient[RequestT, ResponseT]](_.send(request))

  def sendAndUse[R, T](request: HttpRequest[RequestT])(use: HttpResponse[ResponseT] => HRIO[R & Scope, T]): HRIO[R & HttpClient.ClientT & Logger, T] =
    ZIO.serviceWithZIO[HttpClient[RequestT, ResponseT]](_.sendAndUse(request)(use))

}
