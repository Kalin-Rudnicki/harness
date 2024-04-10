package harness.http.client

import harness.web.HttpCode

final case class HttpResponseParams(
    responseCode: HttpCode,
    headers: Map[String, List[String]],
    contentLength: Option[Long],
)
