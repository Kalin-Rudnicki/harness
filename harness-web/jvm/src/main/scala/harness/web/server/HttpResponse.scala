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

  object earlyReturn {

    def apply(response: HttpResponse): IO[EarlyReturn, Nothing] =
      ZIO.fail(EarlyReturn(response))

    object fromHttpCode {

      def apply(httpCode: HttpCode): IO[EarlyReturn, Nothing] =
        HttpResponse.earlyReturn(HttpResponse.fromHttpCode(httpCode))

      def json(httpCode: HttpCode): IO[EarlyReturn, Nothing] =
        HttpResponse.earlyReturn(HttpResponse.fromHttpCode.json(httpCode))

    }

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
    HttpResponse(JsonEncoder[T].encodeJson(value, None).toString, code).header("content-type", "application/json")

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

  // =====| From HTTP Code |=====

  object fromHttpCode {

    def apply(httpCode: HttpCode): HttpResponse.Found =
      HttpResponse(httpCode.name, httpCode)

    def json(httpCode: HttpCode): HttpResponse.Found =
      HttpResponse.encodeJson(httpCode.name :: Nil, httpCode)

    // =====| By Name |=====

    inline def Continue: HttpResponse.Found = HttpResponse.fromHttpCode(HttpCode.Continue)
    inline def SwitchingProtocols: HttpResponse.Found = HttpResponse.fromHttpCode(HttpCode.SwitchingProtocols)
    inline def Processing: HttpResponse.Found = HttpResponse.fromHttpCode(HttpCode.Processing)

    inline def Ok: HttpResponse.Found = HttpResponse.fromHttpCode(HttpCode.Ok)
    inline def Created: HttpResponse.Found = HttpResponse.fromHttpCode(HttpCode.Created)
    inline def Accepted: HttpResponse.Found = HttpResponse.fromHttpCode(HttpCode.Accepted)
    inline def NonAuthoritativeInformation: HttpResponse.Found = HttpResponse.fromHttpCode(HttpCode.NonAuthoritativeInformation)
    inline def NoContent: HttpResponse.Found = HttpResponse.fromHttpCode(HttpCode.NoContent)
    inline def ResetContent: HttpResponse.Found = HttpResponse.fromHttpCode(HttpCode.ResetContent)
    inline def PartialContent: HttpResponse.Found = HttpResponse.fromHttpCode(HttpCode.PartialContent)
    inline def MultiStatus: HttpResponse.Found = HttpResponse.fromHttpCode(HttpCode.MultiStatus)
    inline def AlreadyReported: HttpResponse.Found = HttpResponse.fromHttpCode(HttpCode.AlreadyReported)
    inline def ImUsed: HttpResponse.Found = HttpResponse.fromHttpCode(HttpCode.ImUsed)

    inline def MultipleChoices: HttpResponse.Found = HttpResponse.fromHttpCode(HttpCode.MultipleChoices)
    inline def MovedPermanently: HttpResponse.Found = HttpResponse.fromHttpCode(HttpCode.MovedPermanently)
    inline def Found: HttpResponse.Found = HttpResponse.fromHttpCode(HttpCode.Found)
    inline def SeeOther: HttpResponse.Found = HttpResponse.fromHttpCode(HttpCode.SeeOther)
    inline def NotModified: HttpResponse.Found = HttpResponse.fromHttpCode(HttpCode.NotModified)
    inline def UseProxy: HttpResponse.Found = HttpResponse.fromHttpCode(HttpCode.UseProxy)
    inline def TemporaryRedirect: HttpResponse.Found = HttpResponse.fromHttpCode(HttpCode.TemporaryRedirect)
    inline def PermanentRedirect: HttpResponse.Found = HttpResponse.fromHttpCode(HttpCode.PermanentRedirect)

    inline def BadRequest: HttpResponse.Found = HttpResponse.fromHttpCode(HttpCode.BadRequest)
    inline def Unauthorized: HttpResponse.Found = HttpResponse.fromHttpCode(HttpCode.Unauthorized)
    inline def PaymentRequired: HttpResponse.Found = HttpResponse.fromHttpCode(HttpCode.PaymentRequired)
    inline def Forbidden: HttpResponse.Found = HttpResponse.fromHttpCode(HttpCode.Forbidden)
    inline def NotFound: HttpResponse.Found = HttpResponse.fromHttpCode(HttpCode.NotFound)
    inline def MethodNotAllowed: HttpResponse.Found = HttpResponse.fromHttpCode(HttpCode.MethodNotAllowed)
    inline def NotAcceptable: HttpResponse.Found = HttpResponse.fromHttpCode(HttpCode.NotAcceptable)
    inline def ProxyAuthenticationRequired: HttpResponse.Found = HttpResponse.fromHttpCode(HttpCode.ProxyAuthenticationRequired)
    inline def RequestTimeout: HttpResponse.Found = HttpResponse.fromHttpCode(HttpCode.RequestTimeout)
    inline def Conflict: HttpResponse.Found = HttpResponse.fromHttpCode(HttpCode.Conflict)
    inline def Gone: HttpResponse.Found = HttpResponse.fromHttpCode(HttpCode.Gone)
    inline def LengthRequired: HttpResponse.Found = HttpResponse.fromHttpCode(HttpCode.LengthRequired)
    inline def PreconditionFailed: HttpResponse.Found = HttpResponse.fromHttpCode(HttpCode.PreconditionFailed)
    inline def PayloadTooLarge: HttpResponse.Found = HttpResponse.fromHttpCode(HttpCode.PayloadTooLarge)
    inline def UriTooLong: HttpResponse.Found = HttpResponse.fromHttpCode(HttpCode.UriTooLong)
    inline def UnsupportedMediaType: HttpResponse.Found = HttpResponse.fromHttpCode(HttpCode.UnsupportedMediaType)
    inline def RangeNotSatisfiable: HttpResponse.Found = HttpResponse.fromHttpCode(HttpCode.RangeNotSatisfiable)
    inline def ExpectationFailed: HttpResponse.Found = HttpResponse.fromHttpCode(HttpCode.ExpectationFailed)
    inline def ImATeapot: HttpResponse.Found = HttpResponse.fromHttpCode(HttpCode.ImATeapot)
    inline def MisdirectedRequest: HttpResponse.Found = HttpResponse.fromHttpCode(HttpCode.MisdirectedRequest)
    inline def UnprocessableEntity: HttpResponse.Found = HttpResponse.fromHttpCode(HttpCode.UnprocessableEntity)
    inline def Locked: HttpResponse.Found = HttpResponse.fromHttpCode(HttpCode.Locked)
    inline def FailedDependency: HttpResponse.Found = HttpResponse.fromHttpCode(HttpCode.FailedDependency)
    inline def TooEarly: HttpResponse.Found = HttpResponse.fromHttpCode(HttpCode.TooEarly)
    inline def UpgradeRequired: HttpResponse.Found = HttpResponse.fromHttpCode(HttpCode.UpgradeRequired)
    inline def PreconditionRequired: HttpResponse.Found = HttpResponse.fromHttpCode(HttpCode.PreconditionRequired)
    inline def TooManyRequests: HttpResponse.Found = HttpResponse.fromHttpCode(HttpCode.TooManyRequests)
    inline def RequestHeaderFieldsTooLarge: HttpResponse.Found = HttpResponse.fromHttpCode(HttpCode.RequestHeaderFieldsTooLarge)
    inline def UnavailableForLegalReasons: HttpResponse.Found = HttpResponse.fromHttpCode(HttpCode.UnavailableForLegalReasons)

    inline def InternalServerError: HttpResponse.Found = HttpResponse.fromHttpCode(HttpCode.InternalServerError)
    inline def NotImplemented: HttpResponse.Found = HttpResponse.fromHttpCode(HttpCode.NotImplemented)
    inline def BadGateway: HttpResponse.Found = HttpResponse.fromHttpCode(HttpCode.BadGateway)
    inline def ServiceUnavailable: HttpResponse.Found = HttpResponse.fromHttpCode(HttpCode.ServiceUnavailable)
    inline def GatewayTimeout: HttpResponse.Found = HttpResponse.fromHttpCode(HttpCode.GatewayTimeout)
    inline def HttpVersionNotSupported: HttpResponse.Found = HttpResponse.fromHttpCode(HttpCode.HttpVersionNotSupported)
    inline def VariantAlsoNegotiates: HttpResponse.Found = HttpResponse.fromHttpCode(HttpCode.VariantAlsoNegotiates)
    inline def InsufficientStorage: HttpResponse.Found = HttpResponse.fromHttpCode(HttpCode.InsufficientStorage)
    inline def LoopDetected: HttpResponse.Found = HttpResponse.fromHttpCode(HttpCode.LoopDetected)
    inline def NotExtended: HttpResponse.Found = HttpResponse.fromHttpCode(HttpCode.NotExtended)
    inline def NetworkAuthenticationRequired: HttpResponse.Found = HttpResponse.fromHttpCode(HttpCode.NetworkAuthenticationRequired)

    // =====| By Code |=====

    inline def `100`: HttpResponse.Found = HttpResponse.fromHttpCode(HttpCode.`100`)
    inline def `101`: HttpResponse.Found = HttpResponse.fromHttpCode(HttpCode.`101`)
    inline def `102`: HttpResponse.Found = HttpResponse.fromHttpCode(HttpCode.`102`)

    inline def `200`: HttpResponse.Found = HttpResponse.fromHttpCode(HttpCode.`200`)
    inline def `201`: HttpResponse.Found = HttpResponse.fromHttpCode(HttpCode.`201`)
    inline def `202`: HttpResponse.Found = HttpResponse.fromHttpCode(HttpCode.`202`)
    inline def `203`: HttpResponse.Found = HttpResponse.fromHttpCode(HttpCode.`203`)
    inline def `204`: HttpResponse.Found = HttpResponse.fromHttpCode(HttpCode.`204`)
    inline def `205`: HttpResponse.Found = HttpResponse.fromHttpCode(HttpCode.`205`)
    inline def `206`: HttpResponse.Found = HttpResponse.fromHttpCode(HttpCode.`206`)
    inline def `207`: HttpResponse.Found = HttpResponse.fromHttpCode(HttpCode.`207`)
    inline def `208`: HttpResponse.Found = HttpResponse.fromHttpCode(HttpCode.`208`)
    inline def `226`: HttpResponse.Found = HttpResponse.fromHttpCode(HttpCode.`226`)

    inline def `300`: HttpResponse.Found = HttpResponse.fromHttpCode(HttpCode.`300`)
    inline def `301`: HttpResponse.Found = HttpResponse.fromHttpCode(HttpCode.`301`)
    inline def `302`: HttpResponse.Found = HttpResponse.fromHttpCode(HttpCode.`302`)
    inline def `303`: HttpResponse.Found = HttpResponse.fromHttpCode(HttpCode.`303`)
    inline def `304`: HttpResponse.Found = HttpResponse.fromHttpCode(HttpCode.`304`)
    inline def `305`: HttpResponse.Found = HttpResponse.fromHttpCode(HttpCode.`305`)
    inline def `307`: HttpResponse.Found = HttpResponse.fromHttpCode(HttpCode.`307`)
    inline def `308`: HttpResponse.Found = HttpResponse.fromHttpCode(HttpCode.`308`)

    inline def `400`: HttpResponse.Found = HttpResponse.fromHttpCode(HttpCode.`400`)
    inline def `401`: HttpResponse.Found = HttpResponse.fromHttpCode(HttpCode.`401`)
    inline def `402`: HttpResponse.Found = HttpResponse.fromHttpCode(HttpCode.`402`)
    inline def `403`: HttpResponse.Found = HttpResponse.fromHttpCode(HttpCode.`403`)
    inline def `404`: HttpResponse.Found = HttpResponse.fromHttpCode(HttpCode.`404`)
    inline def `405`: HttpResponse.Found = HttpResponse.fromHttpCode(HttpCode.`405`)
    inline def `406`: HttpResponse.Found = HttpResponse.fromHttpCode(HttpCode.`406`)
    inline def `407`: HttpResponse.Found = HttpResponse.fromHttpCode(HttpCode.`407`)
    inline def `408`: HttpResponse.Found = HttpResponse.fromHttpCode(HttpCode.`408`)
    inline def `409`: HttpResponse.Found = HttpResponse.fromHttpCode(HttpCode.`409`)
    inline def `410`: HttpResponse.Found = HttpResponse.fromHttpCode(HttpCode.`410`)
    inline def `411`: HttpResponse.Found = HttpResponse.fromHttpCode(HttpCode.`411`)
    inline def `412`: HttpResponse.Found = HttpResponse.fromHttpCode(HttpCode.`412`)
    inline def `413`: HttpResponse.Found = HttpResponse.fromHttpCode(HttpCode.`413`)
    inline def `414`: HttpResponse.Found = HttpResponse.fromHttpCode(HttpCode.`414`)
    inline def `415`: HttpResponse.Found = HttpResponse.fromHttpCode(HttpCode.`415`)
    inline def `416`: HttpResponse.Found = HttpResponse.fromHttpCode(HttpCode.`416`)
    inline def `417`: HttpResponse.Found = HttpResponse.fromHttpCode(HttpCode.`417`)
    inline def `418`: HttpResponse.Found = HttpResponse.fromHttpCode(HttpCode.`418`)
    inline def `421`: HttpResponse.Found = HttpResponse.fromHttpCode(HttpCode.`421`)
    inline def `422`: HttpResponse.Found = HttpResponse.fromHttpCode(HttpCode.`422`)
    inline def `423`: HttpResponse.Found = HttpResponse.fromHttpCode(HttpCode.`423`)
    inline def `424`: HttpResponse.Found = HttpResponse.fromHttpCode(HttpCode.`424`)
    inline def `425`: HttpResponse.Found = HttpResponse.fromHttpCode(HttpCode.`425`)
    inline def `426`: HttpResponse.Found = HttpResponse.fromHttpCode(HttpCode.`426`)
    inline def `428`: HttpResponse.Found = HttpResponse.fromHttpCode(HttpCode.`428`)
    inline def `429`: HttpResponse.Found = HttpResponse.fromHttpCode(HttpCode.`429`)
    inline def `431`: HttpResponse.Found = HttpResponse.fromHttpCode(HttpCode.`431`)
    inline def `451`: HttpResponse.Found = HttpResponse.fromHttpCode(HttpCode.`451`)

    inline def `500`: HttpResponse.Found = HttpResponse.fromHttpCode(HttpCode.`500`)
    inline def `501`: HttpResponse.Found = HttpResponse.fromHttpCode(HttpCode.`501`)
    inline def `502`: HttpResponse.Found = HttpResponse.fromHttpCode(HttpCode.`502`)
    inline def `503`: HttpResponse.Found = HttpResponse.fromHttpCode(HttpCode.`503`)
    inline def `504`: HttpResponse.Found = HttpResponse.fromHttpCode(HttpCode.`504`)
    inline def `505`: HttpResponse.Found = HttpResponse.fromHttpCode(HttpCode.`505`)
    inline def `506`: HttpResponse.Found = HttpResponse.fromHttpCode(HttpCode.`506`)
    inline def `507`: HttpResponse.Found = HttpResponse.fromHttpCode(HttpCode.`507`)
    inline def `508`: HttpResponse.Found = HttpResponse.fromHttpCode(HttpCode.`508`)
    inline def `510`: HttpResponse.Found = HttpResponse.fromHttpCode(HttpCode.`510`)
    inline def `511`: HttpResponse.Found = HttpResponse.fromHttpCode(HttpCode.`511`)

  }

}
