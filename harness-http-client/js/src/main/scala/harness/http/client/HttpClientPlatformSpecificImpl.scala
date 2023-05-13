package harness.http.client

import harness.core.*
import harness.zio.*
import zio.*

trait HttpClientPlatformSpecificImpl { self: HttpClientPlatformSpecific =>

  override type RequestT = JsClient.RequestT
  override type ResponseT = JsClient.ResponseT

  override val defaultLayer: HTaskLayer[HttpClient[RequestT, ResponseT]] = JsClient.layer

}
