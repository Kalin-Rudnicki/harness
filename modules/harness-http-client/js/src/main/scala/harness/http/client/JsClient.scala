package harness.http.client

import harness.web.*
import harness.zio.*
import org.scalajs.dom.File
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

  private inline def makeXHR: Task[XMLHttpRequest] =
    ZIO.attempt { new XMLHttpRequest }

  private inline def openXHR(xhr: XMLHttpRequest, method: HttpMethod, url: String, queryParams: Map[String, String]): Task[Unit] =
    ZIO.attempt {
      xhr.open(
        method = method.method,
        url = makeUrl(url, queryParams),
        async = true,
      )
    }

  private inline def setHeaders(xhr: XMLHttpRequest, headers: Map[String, List[String]]): Task[Unit] =
    ZIO.foreachDiscard(headers.toList) { (k, vs) => ZIO.foreachDiscard(vs) { v => ZIO.attempt { xhr.setRequestHeader(k, v) } } }

  private inline def getResponse(xhr: XMLHttpRequest): Task[HttpResponse.Result[JsClient.ResponseT]] =
    for {
      responseCode: HttpCode <- ZIO.attempt { xhr.status }.mapBoth(new RuntimeException("Error getting response code", _), HttpCode(_))
      body: JsClient.ResponseT <- ZIO.attempt { xhr.responseText }
      // TODO (KR) : response headers
      // TODO (KR) : content length
    } yield HttpResponse.Result[JsClient.ResponseT](
      HttpResponse.ResultFields.make[JsClient.ResponseT](responseCode, Map.empty, None, body),
      JsClient.bodyOps,
    )

  private inline def setReturn(xhr: XMLHttpRequest, register: RIO[Logger, HttpResponse.Result[JsClient.ResponseT]] => Unit): Task[Unit] =
    ZIO.attempt {
      xhr.onload = { _ => register(getResponse(xhr)) }
    }

  private inline def send(xhr: XMLHttpRequest, body: Option[JsClient.RequestT]): RIO[Logger, Unit] =
    body match {
      case Some(body: String) => ZIO.attempt { xhr.send(body) }
      case Some(body: File)   => ZIO.attempt { xhr.send(body) }
      case None               => ZIO.attempt { xhr.send() }
    }

  override protected def sendImpl(request: HttpRequest[JsClient.RequestT]): RIO[Logger & Scope, HttpResponse.Result[JsClient.ResponseT]] =
    ZIO.asyncZIO[Logger, Throwable, HttpResponse.Result[JsClient.ResponseT]] { register =>
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

  type RequestT = String | File // TODO (KR) : Make `js.Any`?
  type ResponseT = String // TODO (KR) : Make `Any`?

  val client: HttpClient.ClientT = new JsClient
  val layer: ULayer[HttpClient.ClientT] = ZLayer.succeed(JsClient.client)

  private val bodyOps: HttpResponse.BodyOps[JsClient.ResponseT] =
    HttpResponse.BodyOps.forStringBody

}
