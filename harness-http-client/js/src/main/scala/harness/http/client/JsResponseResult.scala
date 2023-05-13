package harness.http.client

import harness.core.*
import harness.web.*
import harness.zio.*
import zio.*

final case class JsResponseResult(
    responseCode: HttpCode,
    _headers: Map[String, List[String]],
    _contentLength: Option[Long],
    body: JsClient.ResponseT,
) extends HttpResponse.Result[JsClient.ResponseT] { self =>

  override def bodyAsStringImpl(body: JsClient.ResponseT): HRIO[Logger, String] = ZIO.succeed(body)

  override def forwardBodyToPath(path: Path): HTask[Long] = ZIO.fail(HError.???("JsResponseResult#forwardBodyToPath"))

}
