package harness.web.server

import harness.core.*
import harness.web.*
import harness.zio.*
import java.io.OutputStream
import zio.*
import zio.json.JsonEncoder

sealed trait HttpResponse
object HttpResponse {

  case object NotFound extends HttpResponse
  final case class Found private[HttpResponse] (
      code: HttpCode,
      length: Long,
      write: OutputStream => Unit,
      headers: Map[String, String],
      cookies: List[Cookie],
  ) extends HttpResponse { self =>

    // =====| headers |=====

    def header[T: StringEncoder](header: String, value: T): Found =
      self.copy(headers = self.headers + (header -> StringEncoder[T].encode(value)))

    def optHeader[T: StringEncoder](header: String, value: Option[T]): Found =
      value.fold(self)(self.header[T](header, _))

    def jsonHeader[T: JsonEncoder](header: String, value: T): Found =
      self.copy(headers = self.headers + (header -> JsonEncoder[T].encodeJson(value, None).toString))

    def optJsonHeader[T: JsonEncoder](header: String, value: Option[T]): Found =
      value.fold(self)(self.jsonHeader[T](header, _))

    // =====| cookies |=====

    def withCookie(cookie: Cookie): Found =
      self.copy(cookies = cookie :: cookies)

  }

  def apply(value: String, code: HttpCode = HttpCode.`200`): HttpResponse.Found =
    Found(
      code,
      value.length,
      os => os.write(value.getBytes),
      Map.empty,
      Nil,
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
      Nil,
    )

  def genericFile(path: Path, code: HttpCode = HttpCode.`200`)(onDNE: => SHRIO[Scope, HttpResponse]): SHRIO[Scope, HttpResponse] =
    path.exists.flatMap {
      case true =>
        for {
          length <- path.size
          inputStream <- path.inputStream
        } yield HttpResponse.Found(
          code = code,
          length = length,
          write = inputStream.transferTo,
          headers = Map.empty,
          cookies = Nil,
        )
      case false => onDNE
    }

  def fileOrNotFound(path: Path, code: HttpCode = HttpCode.`200`): SHRIO[Scope, HttpResponse] =
    genericFile(path, code) { ZIO.succeed(HttpResponse.NotFound) }

  def fileOrFail(path: Path, code: HttpCode = HttpCode.`200`): SHRIO[Scope, HttpResponse] =
    genericFile(path, code) { ZIO.fail(HError.UserError("No such file", s"No file @ ${path.pathName}")) }

  // TODO (KR) : jar resource

}
