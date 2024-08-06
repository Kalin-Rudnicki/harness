package harness.http.client

import harness.endpoint.spec.*
import harness.endpoint.types.*
import harness.endpoint.types.Types.*
import harness.zio.*
import zio.*

trait HttpClient { self =>

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
  ): ZIO[Scope, Error[ET], Receive[OutputBody[ET]]] =
    self
      .send_internal(inputBodySchema, outputBodySchema, errorSchema)(request, body)
      .telemetrize("HTTP Client Send", "url" -> request.url, "path" -> request.paths.mkString("/", "/", ""))

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
