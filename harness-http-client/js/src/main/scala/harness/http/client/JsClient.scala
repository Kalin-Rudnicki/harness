package harness.http.client

import harness.core.*
import harness.web.*
import harness.zio.*
import org.scalajs.dom.XMLHttpRequest
import scala.scalajs.js.URIUtils.encodeURIComponent
import zio.*

final class JsClient extends HttpClient[JsClient.RequestT, JsClient.ResponseT] {

  private inline def encodeQueryParam(key: String, value: String): String =
    s"${encodeURIComponent(key)}=${encodeURIComponent(value)}"

  private inline def encodeQueryParams(queryParams: Map[String, String]): String =
    if (queryParams.isEmpty) ""
    else queryParams.toList.map(encodeQueryParam(_, _)).mkString("?", "&", "")

  private inline def makeUrl(url: String, queryParams: Map[String, String]): String =
    s"$url${encodeQueryParams(queryParams)}"

  private inline def makeXHR: HTask[XMLHttpRequest] =
    ZIO.hAttempt { new XMLHttpRequest }

  private inline def openXHR(xhr: XMLHttpRequest, method: HttpMethod, url: String, queryParams: Map[String, String]): HTask[Unit] =
    ZIO.hAttempt {
      xhr.open(
        method = method.method,
        url = makeUrl(url, queryParams),
        async = true,
      )
    }

  private inline def setHeaders(xhr: XMLHttpRequest, headers: Map[String, List[String]]): HTask[Unit] =
    ZIO.foreachDiscard(headers.toList) { (k, vs) => ZIO.foreachDiscard(vs) { v => ZIO.hAttempt { xhr.setRequestHeader(k, v) } } }

  private inline def getResponse(xhr: XMLHttpRequest): HTask[HttpResponse.Result[JsClient.ResponseT]] =
    for {
      responseCode: HttpCode <- ZIO.hAttempt { xhr.status }.mapBoth(HError.SystemFailure("Error getting response code", _), HttpCode(_))
      body: JsClient.ResponseT <- ZIO.hAttempt { xhr.responseText }
      // TODO (KR) : response headers
      // TODO (KR) : content length
    } yield JsResponseResult(responseCode, Map.empty, None, body)

  private inline def setReturn(xhr: XMLHttpRequest, register: HRIO[Logger, HttpResponse.Result[JsClient.ResponseT]] => Unit): HTask[Unit] =
    ZIO.hAttempt {
      xhr.onload = { _ => register(getResponse(xhr)) }
    }

  private inline def send(xhr: XMLHttpRequest, body: Option[JsClient.RequestT]): HTask[Unit] =
    body match {
      case Some(body) => ZIO.hAttempt { xhr.send(body) }
      case None       => ZIO.hAttempt { xhr.send() }
    }

  override protected def sendImpl(request: HttpRequest[JsClient.RequestT]): HRIO[Logger & Scope, HttpResponse.Result[JsClient.ResponseT]] =
    ZIO.asyncZIO[Logger, HError, HttpResponse.Result[JsClient.ResponseT]] { register =>
      for {
        xhr <- makeXHR
        _ <- openXHR(xhr, request.method, request.url, request.queryParams)
        _ <- setHeaders(xhr, request.headers)
        _ <- setReturn(xhr, register)
        _ <- send(xhr, request.body)
      } yield ()
    }

}
object JsClient {

  type RequestT = String // TODO (KR) : Make `js.Any`?
  type ResponseT = String // TODO (KR) : Make `Any`?

  val layer: ULayer[HttpClient.ClientT] = ZLayer.succeed(new JsClient)

}
