package harness.http.client

import harness.zio.*
import zio.*

trait HttpClientPlatformSpecificImpl { self: HttpClientPlatformSpecific =>

  override type RequestT = JavaClient.RequestT
  override type ResponseT = JavaClient.ResponseT

  override val defaultClient: ClientT = JavaClient.client
  override val defaultLayer: ULayer[ClientT] = ZLayer.succeed(defaultClient)

}
