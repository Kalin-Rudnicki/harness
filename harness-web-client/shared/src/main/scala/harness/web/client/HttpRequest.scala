package harness.web.client

import cats.syntax.option.*
import harness.core.StringEncoder
import harness.web.client
import zio.json.JsonEncoder

final case class HttpRequest[+B](
    method: HttpMethod,
    url: String,
    queryParams: Map[String, String],
    headers: Map[String, List[String]],
    body: Option[B],
)
object HttpRequest {

  def apply(method: HttpMethod): HttpRequest.Builder1 = HttpRequest.Builder1(method)
  def apply(method: HttpMethod, url: String): HttpRequest.Builder2 = HttpRequest.Builder2(method, url, Map.empty, Map.empty)

  inline def get: HttpRequest.Builder1 = HttpRequest(HttpMethod.GET)
  inline def post: HttpRequest.Builder1 = HttpRequest(HttpMethod.POST)
  inline def put: HttpRequest.Builder1 = HttpRequest(HttpMethod.PUT)
  inline def delete: HttpRequest.Builder1 = HttpRequest(HttpMethod.DELETE)
  inline def head: HttpRequest.Builder1 = HttpRequest(HttpMethod.HEAD)
  inline def options: HttpRequest.Builder1 = HttpRequest(HttpMethod.OPTIONS)
  inline def connect: HttpRequest.Builder1 = HttpRequest(HttpMethod.CONNECT)
  inline def trace: HttpRequest.Builder1 = HttpRequest(HttpMethod.TRACE)
  inline def patch: HttpRequest.Builder1 = HttpRequest(HttpMethod.PATCH)

  final class Builder1(
      method: HttpMethod,
  ) {
    def apply(url: String): HttpRequest.Builder2 = HttpRequest.Builder2(method, url, Map.empty, Map.empty)
  }

  final class Builder2(
      method: HttpMethod,
      url: String,
      queryParams: Map[String, String],
      headers: Map[String, List[String]],
  ) { self =>

    def withQueryParam(key: String, value: String): HttpRequest.Builder2 = HttpRequest.Builder2(method, url, queryParams.updated(key, value), headers)
    inline def withQueryParamEncoded[A: StringEncoder](key: String, value: A): HttpRequest.Builder2 = self.withQueryParam(key, StringEncoder[A].encode(value))

    def withHeader(key: String, value: String): HttpRequest.Builder2 = HttpRequest.Builder2(method, url, queryParams, headers.updated(key, value :: Nil))
    inline def withHeaderEncoded[A: StringEncoder](key: String, value: A): HttpRequest.Builder2 = self.withHeader(key, StringEncoder[A].encode(value))

    def addHeader(key: String, value: String): HttpRequest.Builder2 = HttpRequest.Builder2(method, url, queryParams, headers.updatedWith(key) { current => (value :: current.getOrElse(Nil)).some })
    inline def addHeaderEncoded[A: StringEncoder](key: String, value: A): HttpRequest.Builder2 = self.addHeader(key, StringEncoder[A].encode(value))

    // TODO (KR) : cookies

    def withNoBody: HttpRequest[Nothing] = HttpRequest(method, url, queryParams, headers, None)
    def withStringBody(body: String): HttpRequest[String] = HttpRequest(method, url, queryParams, headers, body.some)
    inline def withBodyEncoded[B: StringEncoder](body: B): HttpRequest[String] = self.withStringBody(StringEncoder[B].encode(body))
    inline def withBodyJsonEncoded[B: JsonEncoder](body: B): HttpRequest[String] = self.withStringBody(JsonEncoder[B].encodeJson(body, None).toString)
    def withBody[B](body: B): HttpRequest[B] = HttpRequest(method, url, queryParams, headers, body.some)

  }

}
