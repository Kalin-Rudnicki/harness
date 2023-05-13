package harness.web.client

import harness.core.*
import harness.zio.*
import zio.*

trait HttpClientPlatformSpecificImpl { self: HttpClientPlatformSpecific =>

  override type RequestT = Any
  override type ResponseT = Any // TODO (KR) :

  override val defaultLayer: HTaskLayer[HttpClient[RequestT, ResponseT]] =
    ZLayer.fromZIO { ZIO.fail(HError.???("Creating JS HTTP Client")) }

}
