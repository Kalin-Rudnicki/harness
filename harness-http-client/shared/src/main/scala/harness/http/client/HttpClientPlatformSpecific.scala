package harness.http.client

import harness.core.*
import harness.zio.*
import zio.*

trait HttpClientPlatformSpecific {

  type RequestT
  type ResponseT
  final type ClientT = HttpClient[RequestT, ResponseT]

  val defaultLayer: HTaskLayer[HttpClient[RequestT, ResponseT]]

}
