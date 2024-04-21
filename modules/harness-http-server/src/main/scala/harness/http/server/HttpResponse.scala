package harness.http.server

import harness.web.HttpCode

final case class HttpResponse[B](
    body: B,
    code: HttpCode,
    headers: Map[String, List[String]],
    cookies: List[SetCookie],
) { self =>

  private def modifyHeader(k: String)(vF: List[String] => List[String]): HttpResponse[B] =
    self.copy(
      headers = headers.updatedWith(k) { vs =>
        val res = vF(vs.getOrElse(Nil))
        Option.when(res.nonEmpty)(res)
      },
    )

  def withHeader(k: String, v: String): HttpResponse[B] =
    modifyHeader(k) { _ => v :: Nil }
  def withHeaders(k: String, vs: List[String]): HttpResponse[B] =
    modifyHeader(k) { _ => vs }

  def addHeader(k: String, v: String): HttpResponse[B] =
    modifyHeader(k) { _ :+ v }
  def addHeaders(k: String, vs: List[String]): HttpResponse[B] =
    modifyHeader(k) { _ ++ vs }

  def withCookie(cookie: SetCookie): HttpResponse[B] =
    self.copy(cookies = cookie :: self.cookies)

}
object HttpResponse {

  def apply[B](body: B, code: HttpCode = HttpCode.`200`): HttpResponse[B] =
    HttpResponse(body, code, Map.empty, Nil)

  def redirect(location: String, code: HttpCode = HttpCode.PermanentRedirect): HttpResponse[Unit] =
    HttpResponse((), code).withHeader("Location", location)

}
