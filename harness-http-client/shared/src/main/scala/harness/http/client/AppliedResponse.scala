package harness.http.client

import harness.zio.*
import zio.*

final class AppliedResponse(request: HttpRequest[HttpClient.RequestT]) extends ResponseOps.Builder1[HttpClient.ClientT & Logger & Telemetry, HttpClient.ResponseT] {
  override protected def getResponse: HRIO[HttpClient.ClientT & Logger & Telemetry & Scope, HttpResponse[HttpClient.ResponseT]] = HttpClient.send(request)
}
