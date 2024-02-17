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

  private inline def makeUrl(url: String, queryParams: Map[String, String]): RIO[Logger, URL] =
    ZIO.attempt { new URL(s"$url${encodeQueryParams(queryParams)}") }

  private inline def getConnection(url: URL): RIO[Scope, HttpURLConnection] =
    ZIO.attempt { url.openConnection() }.mapError(new RuntimeException(s"Error opening URL connection for: $url", _)).flatMap {
      case httpURLConnection: HttpURLConnection => ZIO.acquireRelease(ZIO.succeed(httpURLConnection)) { con => ZIO.attempt { con.disconnect() }.orDie }
      case tmpCon                               => ZIO.fail(new RuntimeException(s"Expected HttpURLConnection, but got: ${tmpCon.getClass.getName}"))
    }

  private inline def setMethod(con: HttpURLConnection, method: HttpMethod): Task[Unit] =
    ZIO.attempt { con.setRequestMethod(method.method) }

  private inline def setHeaders(con: HttpURLConnection, headers: Map[String, List[String]]): Task[Unit] =
    ZIO.foreachDiscard(headers.toList) {
      case (header, value :: Nil) => ZIO.attempt { con.setRequestProperty(header, value) }
      case (header, values)       => ZIO.foreachDiscard(values) { value => ZIO.attempt { con.addRequestProperty(header, value) } }
    }

  private inline def setBody(con: HttpURLConnection, body: JavaClient.RequestT): Task[Unit] =
    ZIO.scoped {
      for {
        os: java.io.OutputStream <- ZIO.fromAutoCloseable(ZIO.attempt(con.getOutputStream))
        _ <- ZIO.attempt { os.write(body.getBytes) }.mapError(new RuntimeException("Error writing request body", _))
        _ <- ZIO.attempt { os.flush() }.mapError(new RuntimeException("Error flushing request body", _))
      } yield ()
    }

  private inline def getResponse(con: HttpURLConnection): RIO[Logger & Scope, HttpResponse.Result[JavaClient.ResponseT]] =
    for {
      responseCode: HttpCode <- ZIO.attempt { con.getResponseCode() }.mapBoth(new RuntimeException("Error getting response code", _), HttpCode(_))
      contentLength: Long <- ZIO.attempt { con.getContentLengthLong() }.mapError(new RuntimeException("Error getting content length", _))
      javaHeaders: java.util.Map[String, java.util.List[String]] <- ZIO.attempt { con.getHeaderFields() }
      scalaHeaders: Map[String, List[String]] = javaHeaders.asScala.toList.flatMap { case (k, v) => Option(k).map(k => (k.toLowerCase, v.asScala.toList)) }.toMap
      body: JavaClient.ResponseT <-
        if (responseCode.is2xx) ZIO.acquireClosable { ZIO.attempt { con.getInputStream() } }
        else if (responseCode.is4xxOr5xx) ZIO.acquireClosable { ZIO.attempt { con.getErrorStream() } }
        else
          Logger.log.warning("Response is not 2xx, 4xx, or 5xx. Assuming to use 'inputStream' and not 'errorStream'") *>
            ZIO.acquireClosable { ZIO.attempt { con.getInputStream() } }
    } yield HttpResponse.Result[JavaClient.ResponseT](
      HttpResponse.ResultFields.make[JavaClient.ResponseT](responseCode, scalaHeaders, contentLength.some, body),
      JavaClient.bodyOps,
    )

  override def sendImpl(request: HttpRequest[JavaClient.RequestT]): RIO[Logger & Scope, HttpResponse.Result[JavaClient.ResponseT]] =
    for {
      url <- makeUrl(request.url, request.queryParams)
      _ <- Logger.log.debug(s"Sending HTTP request to: $url")
      con: HttpURLConnection <- getConnection(url)
      _ <- setMethod(con, request.method)
      _ <- ZIO.attempt { con.setDoOutput(true) }
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
                ZIO.attempt { fields.body.readNBytes(cl) }
            case None =>
              Logger.log.warning("Reading body without content length") *>
                ZIO.attempt { fields.body.readAllBytes() }
          }
          bytes: Array[Byte] <- getBytes.mapError(new RuntimeException("Error getting bytes for response body", _))
        } yield new String(bytes),
      (path, body) =>
        ZIO.scoped {
          path.outputStream.flatMap { os =>
            ZIO.attempt { body.transferTo(os) }.mapError(new RuntimeException(s"Error forwarding response body to path $path", _))
          }
        },
    )

}
