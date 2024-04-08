package harness.http.client
import harness.endpoint.spec.*
import harness.endpoint.types.*
import harness.endpoint.types.Types.*
import harness.zio.*
import zio.*

trait HttpClientPlatformSpecificImpl { self: HttpClientPlatformSpecific =>

  override val defaultClient: HttpClient =
    new HttpClient {

      override def send_internal[ET <: EndpointType.Any](
          inputBodySchema: BodySchema[InputBody[ET]],
          outputBodySchema: BodySchema[OutputBody[ET]],
          errorSchema: ErrorSchema[Error[ET]],
      )(
          request: HttpRequestParams,
          body: Send[InputBody[ET]],
      ): ZIO[Logger & Telemetry, Error[ET], Receive[OutputBody[ET]]] =
        ???

    }

}
