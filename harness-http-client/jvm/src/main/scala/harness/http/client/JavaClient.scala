package harness.http.client

import cats.syntax.option.*
import harness.core.*
import harness.web.*
import harness.zio.*
import java.io.InputStream
import java.net.{HttpURLConnection, URL, URLEncoder}
import scala.jdk.CollectionConverters.*
import zio.*

final class JavaClient extends HttpClient[JavaClient.RequestT, JavaClient.ResponseT] {

  private inline def encodeQueryParam(key: String, value: String): String =
    s"${URLEncoder.encode(key, "UTF-8")}=${URLEncoder.encode(value, "UTF-8")}"

  private inline def encodeQueryParams(queryParams: Map[String, String]): String =
    if (queryParams.isEmpty) ""
    else queryParams.toList.map(encodeQueryParam(_, _)).mkString("?", "&", "")

  private inline def makeUrl(url: String, queryParams: Map[String, String]): HRIO[Logger, URL] =
    ZIO.hAttempt { new URL(s"$url${encodeQueryParams(queryParams)}") }

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

  private inline def getResponse(con: HttpURLConnection): HRIO[Logger & Scope, HttpResponse.Result[JavaClient.ResponseT]] =
    for {
      responseCode: HttpCode <- ZIO.hAttempt { con.getResponseCode() }.mapBoth(HError.SystemFailure("Error getting response code", _), HttpCode(_))
      contentLength: Long <- ZIO.hAttempt { con.getContentLengthLong() }.mapError(HError.SystemFailure("Error getting content length", _))
      javaHeaders: java.util.Map[String, java.util.List[String]] <- ZIO.hAttempt { con.getHeaderFields() }
      scalaHeaders: Map[String, List[String]] = javaHeaders.asScala.toList.flatMap { case (k, v) => Option(k).map(k => (k.toLowerCase, v.asScala.toList)) }.toMap
      body: JavaClient.ResponseT <-
        if (responseCode.is2xx) ZIO.acquireClosable { ZIO.hAttempt { con.getInputStream() } }
        else if (responseCode.is4xxOr5xx) ZIO.acquireClosable { ZIO.hAttempt { con.getErrorStream() } }
        else
          Logger.log.warning("Response is not 2xx, 4xx, or 5xx. Assuming to use 'inputStream' and not 'errorStream'") *>
            ZIO.acquireClosable { ZIO.hAttempt { con.getInputStream() } }
    } yield HttpResponse.Result[JavaClient.ResponseT](
      HttpResponse.ResultFields.make[JavaClient.ResponseT](responseCode, scalaHeaders, contentLength.some, body),
      JavaClient.bodyOps,
    )

  override def sendImpl(request: HttpRequest[JavaClient.RequestT]): HRIO[Logger & Scope, HttpResponse.Result[JavaClient.ResponseT]] =
    for {
      url <- makeUrl(request.url, request.queryParams)
      _ <- Logger.log.debug(s"Sending HTTP request to: $url")
      con: HttpURLConnection <- getConnection(url)
      _ <- setMethod(con, request.method)
      _ <- ZIO.hAttempt { con.setDoOutput(true) }
      _ <- setHeaders(con, request.headers)
      _ <- ZIO.foreachDiscard(request.body)(setBody(con, _))
      response <- getResponse(con)
    } yield response

}
object JavaClient {

  type RequestT = String
  type ResponseT = InputStream

  val client: HttpClient.ClientT = new JavaClient
  val layer: ULayer[HttpClient.ClientT] = ZLayer.succeed(JavaClient.client)

  private val bodyOps: HttpResponse.BodyOps[JavaClient.ResponseT] =
    HttpResponse.BodyOps[JavaClient.ResponseT](
      fields =>
        for {
          cl <- fields.contentLengthInt
          getBytes = cl match {
            case Some(cl) =>
              Logger.log.debug(s"Reading body with content length of ${cl.toStringCommas}") *>
                ZIO.hAttempt { fields.body.readNBytes(cl) }
            case None =>
              Logger.log.warning("Reading body without content length") *>
                ZIO.hAttempt { fields.body.readAllBytes() }
          }
          bytes: Array[Byte] <- getBytes.mapError(HError.SystemFailure("Error getting bytes for response body", _))
        } yield new String(bytes),
      (path, body) =>
        ZIO.scoped {
          path.outputStream.flatMap { os =>
            ZIO.hAttempt { body.transferTo(os) }.mapError(HError.SystemFailure(s"Error forwarding response body to path $path", _))
          }
        },
    )

}
