package harness.http.client

import harness.zio.*
import zio.*

trait HttpClientPlatformSpecificImpl { self: HttpClientPlatformSpecific =>

  override type RequestT = JavaClient.RequestT
  override type ResponseT = JavaClient.ResponseT

  override val defaultLayer: HTaskLayer[HttpClient[RequestT, ResponseT]] = JavaClient.layer

}
