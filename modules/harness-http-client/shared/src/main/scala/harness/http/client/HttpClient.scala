package harness.http.client

import harness.endpoint.spec.*
import harness.endpoint.types.*
import harness.endpoint.types.Types.*
import harness.serviceTracer.TracedService
import harness.zio.*
import zio.*

abstract class HttpClient extends TracedService.Auto { self =>

  protected def send_internal[ET <: EndpointType.Any](
      inputBodySchema: BodyCodec[InputBody[ET]],
      outputBodySchema: BodyCodec[OutputBody[ET]],
      errorSchema: ErrorSchema[Error[ET]],
  )(
      request: HttpRequestParams,
      body: Send[InputBody[ET]],
  ): ZIO[Scope, Error[ET], Receive[OutputBody[ET]]]

  final def send[ET <: EndpointType.Any](
      inputBodySchema: BodyCodec[InputBody[ET]],
      outputBodySchema: BodyCodec[OutputBody[ET]],
      errorSchema: ErrorSchema[Error[ET]],
  )(
      request: HttpRequestParams,
      body: Send[InputBody[ET]],
  ): ZIO[Scope, Error[ET], Receive[OutputBody[ET]]] = {
    val pathString = request.paths.mkString("/", "/", "")
    self.send_internal(inputBodySchema, outputBodySchema, errorSchema)(request, body) @@
      Telemetry.telemetrize("HTTP Client Send", Logger.LogLevel.Trace, "url" -> request.url, "path" -> pathString) @@
      trace("url" -> request.url, "path" -> pathString)
  }

}
object HttpClient extends HttpClientPlatformSpecific with HttpClientPlatformSpecificImpl {

  private[client] val httpClientRef: FiberRef[HttpClient] =
    Unsafe.unsafely {
      FiberRef.unsafe.make[HttpClient](
        defaultClient,
        identity,
        (_, child) => child,
      )
    }

  // =====| API |=====

  def withHttpClient(f: HttpClient): FiberRefModification =
    HttpClient.httpClientRef.modification.set(f)

  def withHttpClient(f: HttpClient => HttpClient): FiberRefModification =
    HttpClient.httpClientRef.modification.update(f)

}
