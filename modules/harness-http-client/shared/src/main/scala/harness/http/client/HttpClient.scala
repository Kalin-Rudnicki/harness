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
  ): ZIO[Logger & Telemetry & Scope, Error[ET], Receive[OutputBody[ET]]]

  final def send[ET <: EndpointType.Any](
      inputBodySchema: BodyCodec[InputBody[ET]],
      outputBodySchema: BodyCodec[OutputBody[ET]],
      errorSchema: ErrorSchema[Error[ET]],
  )(
      request: HttpRequestParams,
      body: Send[InputBody[ET]],
  ): ZIO[Logger & Telemetry & Scope, Error[ET], Receive[OutputBody[ET]]] =
    self
      .send_internal(inputBodySchema, outputBodySchema, errorSchema)(request, body)
      .telemetrize("HTTP Client Send", "url" -> request.url, "path" -> request.paths.mkString("/", "/", ""))

}
object HttpClient extends HttpClientPlatformSpecific with HttpClientPlatformSpecificImpl {

  final val defaultLayer: ULayer[HttpClient] = ZLayer.succeed { defaultClient }

}
