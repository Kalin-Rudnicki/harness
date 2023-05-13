package harness.web.client

import cats.syntax.option.*
import harness.core.*
import harness.zio.*
import java.io.InputStream
import java.net.{HttpURLConnection, URL, URLConnection}
import scala.jdk.CollectionConverters.*
import zio.*

final class JavaClient extends HttpClient[JavaClient.RequestT, JavaClient.ResponseT] {

  private inline def makeUrl(url: String, queryParams: Map[String, String]): HRIO[Logger, URL] = // TODO (KR) :
    Logger.log.warning(s"Not setting query params for URL...${queryParams.toList.map { (k, v) => s"\n  - $k : $v" }.mkString}").as(new URL(url))

  private inline def getConnection(url: URL): HRIO[Scope, HttpURLConnection] =
    ZIO.hAttempt { url.openConnection() }.mapError(HError.SystemFailure(s"Error opening URL connection for: $url", _)).flatMap {
      case httpURLConnection: HttpURLConnection => ZIO.acquireRelease(ZIO.succeed(httpURLConnection)) { con => ZIO.hAttempt { con.disconnect() }.collapseCause.orDie }
      case tmpCon                               => ZIO.fail(HError.InternalDefect(s"Expected HttpURLConnection, but got: ${tmpCon.getClass.getSimpleName}"))
    }

  private inline def setMethod(con: HttpURLConnection, method: HttpMethod): HTask[Unit] =
    ZIO.hAttempt { con.setRequestMethod(method.method) }

  private inline def setHeaders(con: HttpURLConnection, headers: Map[String, List[String]]): HTask[Unit] =
    ZIO.foreachDiscard(headers.toList) {
      case (header, value :: Nil) => ZIO.hAttempt { con.setRequestProperty(header, value) }
      case (header, values)       => ZIO.foreachDiscard(values) { value => ZIO.hAttempt { con.addRequestProperty(header, value) } }
    }

  private inline def setBody(con: HttpURLConnection, body: JavaClient.RequestT): HTask[Unit] =
    ZIO.scoped {
      for {
        os: java.io.OutputStream <- ZIO.fromAutoCloseable(ZIO.hAttempt(con.getOutputStream))
        _ <- ZIO.hAttempt { os.write(body.getBytes) }.mapError(HError.SystemFailure("Error writing request body", _))
        _ <- ZIO.hAttempt { os.flush() }.mapError(HError.SystemFailure("Error flushing request body", _))
      } yield ()
    }

  // TODO (KR) : remove debugging
  private inline def getResponse(con: HttpURLConnection): HRIO[Logger & Scope, HttpResponse.Result[JavaClient.ResponseT]] =
    for {
      responseCode: HttpCode <- ZIO.hAttempt { con.getResponseCode() }.mapBoth(HError.SystemFailure("Error getting response code", _), HttpCode(_))
      _ <- Logger.log.debug(s"Response Code: $responseCode")

      contentLength: Long <- ZIO.hAttempt { con.getContentLengthLong() }.mapError(HError.SystemFailure("Error getting content length", _))
      _ <- Logger.log.debug(s"Content Length: ${contentLength.toStringCommas}")

      javaHeaders: java.util.Map[String, java.util.List[String]] <- ZIO.hAttempt { con.getHeaderFields() }
      scalaHeaders: Map[String, List[String]] = javaHeaders.asScala.toList.flatMap { case (k, v) => Option(k).map(k => (k.toLowerCase, v.asScala.toList)) }.toMap
      _ <- Logger.log.debug(s"Headers:${scalaHeaders.toList.map { (k, v) => s"\n - $k:${v.map(v => s"\n   - $v").mkString}" }.mkString}")

      body: JavaClient.ResponseT <-
        if (responseCode.is2xx) ZIO.acquireClosable { ZIO.hAttempt { con.getInputStream() } }
        else if (responseCode.is4xxOr5xx) ZIO.acquireClosable { ZIO.hAttempt { con.getErrorStream() } }
        else
          Logger.log.warning("Response is not 2xx, 4xx, or 5xx. Assuming to use 'inputStream' and not 'errorStream'") *>
            ZIO.acquireClosable { ZIO.hAttempt { con.getInputStream() } }
    } yield JavaResponseResult(responseCode, scalaHeaders, contentLength.some, body)

  override def sendImpl(request: HttpRequest[JavaClient.RequestT]): HRIO[Logger & Scope, HttpResponse.Result[JavaClient.ResponseT]] =
    for {
      url <- makeUrl(request.url, request.queryParams)
      con: HttpURLConnection <- getConnection(url)
      _ <- setMethod(con, request.method)
      _ <- setHeaders(con, request.headers)
      _ <- ZIO.foreachDiscard(request.body)(setBody(con, _))
      response <- getResponse(con)
    } yield response

}
object JavaClient {

  type RequestT = String
  type ResponseT = InputStream

  val layer: ULayer[HttpClient[JavaClient.RequestT, JavaClient.ResponseT]] = ZLayer.succeed(new JavaClient)

}
