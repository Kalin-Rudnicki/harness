package harness.web.server

import harness.core.*
import java.io.OutputStream
import zio.json.JsonEncoder

sealed trait HttpResponse
object HttpResponse {

  case object NotFound extends HttpResponse
  final case class Found private[HttpResponse] (
      code: HttpCode,
      length: Long,
      write: OutputStream => Unit,
      headers: Map[String, String],
      // TODO (KR) : Cookies
  ) extends HttpResponse { self =>

    def withHeader[T: StringEncoder](header: String, value: T): Found =
      self.copy(headers = self.headers + (header -> StringEncoder[T].encode(value)))

    def withHeaderOpt[T: StringEncoder](header: String, value: Option[T]): Found =
      value.fold(self)(self.withHeader[T](header, _))

    // TODO (KR) : Cookies

  }

  def apply(value: String, code: HttpCode = HttpCode.`200`): HttpResponse.Found =
    Found(
      code,
      value.length,
      os => os.write(value.getBytes),
      Map.empty,
    )

  def encode[T: StringEncoder](value: T, code: HttpCode = HttpCode.`200`): HttpResponse.Found =
    HttpResponse(StringEncoder[T].encode(value), code)

  def encodeJson[T: JsonEncoder](value: T, code: HttpCode = HttpCode.`200`): HttpResponse.Found =
    HttpResponse(JsonEncoder[T].encodeJson(value, None).toString, code)

  def redirect(location: String, code: HttpCode = HttpCode.`301`): HttpResponse.Found =
    Found(
      code,
      0,
      { _ => },
      Map("Location" -> location),
    )

  // TODO (KR) : file

  // TODO (KR) : jar resource

}
