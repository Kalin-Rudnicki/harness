package harness.http.client

import harness.endpoint.types.Types.*
import harness.web.HttpMethod

final case class HttpRequestParams(
    method: HttpMethod,
    url: String,
    paths: List[String],
    queryParams: List[(String, String)],
    headers: Map[String, List[String]],
)
