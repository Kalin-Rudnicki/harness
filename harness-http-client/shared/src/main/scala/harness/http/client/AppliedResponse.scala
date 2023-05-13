package harness.http.client

import harness.zio.*
import zio.*

final class AppliedResponse(request: HttpRequest[HttpClient.RequestT]) extends ResponseOps[HttpClient.ClientT & Logger & Telemetry, Logger, HttpClient.ResponseT] {
  override protected def getResponse: HRIO[HttpClient.ClientT & Logger & Telemetry & Scope, HttpResponse[HttpClient.ResponseT]] = HttpClient.send(request)
}
