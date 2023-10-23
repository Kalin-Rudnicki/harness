package harness.http.client

import harness.core.*
import harness.zio.*
import zio.*

trait HttpClientPlatformSpecificImpl { self: HttpClientPlatformSpecific =>

  override type RequestT = JsClient.RequestT
  override type ResponseT = JsClient.ResponseT

  override val defaultClient: ClientT = JsClient.client
  override val defaultLayer: ULayer[ClientT] = ZLayer.succeed(defaultClient)

}
